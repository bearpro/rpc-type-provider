module RpcTypeExporter.ValueSerialization

open System
open System.Reflection
open System.IO
open Newtonsoft.Json
open RpcTypeExporter.ApiSpecification

type Value =
| Unit
| Integer of int
| Float of float
| String of string
| Bool of bool
| List of valueType: ValueSpec * values: Value list
| Complex of typeName:string * NamedValue list
with
    static member toSpec value =
        match value with
        | Unit -> ValueSpec.Unit
        | Integer _ -> ValueSpec.Integer
        | Float _ -> ValueSpec.Float
        | String _ -> ValueSpec.String
        | Bool _ -> ValueSpec.Bool
        | List (itemSpec, _) -> ValueSpec.List(itemSpec)
        | Complex (typeName, items) -> 
            let itemSpecs = items |> List.map(fun v -> 
                { name = v.name; valueType = Value.toSpec v.value})
            ValueSpec.Complex(typeName, itemSpecs)
and NamedValue = { name: string; value: Value }

type SerializationContext =
    { complexTypeMap: Map<ValueSpec, Type> }

let assertTypeMatched (value: obj) valueSpec =
    let fail() = failwithf "Value %A not matches specification %A" value valueSpec
    if (value = null && valueSpec = ValueSpec.Unit) then
        ()
    else
        let actualValueSpec = getValueSpec(value.GetType())
        if actualValueSpec = valueSpec then () else fail()

let iterateFields (fieldsSpec: Named list) (value: obj) =
    let t = value.GetType()
    let properties = t.GetProperties(BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.GetProperty)
    [ for namedField in fieldsSpec ->
        let matchedProp = Array.Find(properties, fun pi -> pi.Name = namedField.name)
        let propValue = matchedProp.GetValue value
        namedField, propValue ]

let rec serialize context (valueSpec: ValueSpec) (value: obj) : Value * SerializationContext  =
    assertTypeMatched value valueSpec
    match valueSpec with
    | ValueSpec.Unit -> Value.Unit, context
    | ValueSpec.Integer -> Value.Integer (value :?> int), context
    | ValueSpec.Float -> Value.Float (value :?> float), context
    | ValueSpec.String -> Value.String (value :?> string), context
    | ValueSpec.Bool -> Value.Bool (value :?> bool), context
    | ValueSpec.List listItemSpec -> 
        let list = value :?> System.Collections.IEnumerable
        let rec serializeItems context (enumerator: Collections.IEnumerator) =
            if enumerator.MoveNext() then
                let item, ctx = serialize context listItemSpec (enumerator.Current)
                let items, ctx = serializeItems ctx enumerator
                item :: items, ctx
            else [], context

        let items, context = serializeItems context (list.GetEnumerator())
        Value.List(listItemSpec, items), context
    | ValueSpec.Complex(typeName, namedSpec) -> 
        let t = value.GetType()
        let context' = { context with complexTypeMap = context.complexTypeMap.Add(valueSpec, t) }
        let rec serializeFields context (fields: (Named * obj) list) =
            match fields with
            | [] -> [], context
            | (named, value) :: xs -> 
                let item, ctx = serialize context named.valueType value
                let item = { name = named.name; value = item }
                let items, ctx = serializeFields ctx xs
                item :: items, ctx
        let fields, context'' = serializeFields context' (iterateFields namedSpec value)

        Value.Complex(typeName, fields), context''

let rec deserialize (serializationContext: SerializationContext) (value: Value) : obj =
    let ctx = serializationContext
    match value with
    | Value.Unit -> () :> obj
    | Value.Integer x -> x :> obj
    | Value.Float x -> x :> obj
    | Value.String x -> x :> obj
    | Value.Bool x -> x :> obj
    | Value.List(listItemSpec, values) -> 
        let sourceList = values |> List.map (deserialize ctx) |> List.rev
        let mutableListType = typedefof<list<_>>
        let mutableListType' = mutableListType.MakeGenericType(sourceList.Head.GetType())
        let emptyProp = mutableListType'.GetProperty("Empty")
        let consMethod = mutableListType'.GetMethod("Cons")
        let mutable typedList = emptyProp.GetValue(null)
        for item in sourceList do
            typedList <- consMethod.Invoke(null, [| item; typedList |])
        typedList
    | Value.Complex(typeName, fields) -> 
        let deserializedFields = fields |> List.map (fun x -> deserialize ctx x.value) |> Array.ofList
        let deserializedFieldTypes = deserializedFields |> Array.map (fun x -> x.GetType())
        let spec = value |> Value.toSpec
        let t = serializationContext.complexTypeMap |> Map.find spec
        let ctor = t.GetConstructor(deserializedFieldTypes)
        let instance = ctor.Invoke(deserializedFields)
        instance
