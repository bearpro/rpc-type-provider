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

let assertTypeMatched (value: obj) valueSpec =
    let fail() = failwithf "Value %A not matches specification %A" value valueSpec
    if (value = null && valueSpec = ValueSpec.Unit) then
        ()
    else
        let actualValueSpec, _ = getSpec SerializationContext.Empty (value.GetType())
        if actualValueSpec = valueSpec then () else fail()

let iterateFields (fieldsSpec: Named list) (value: obj) =
    let t = value.GetType()
    let properties = t.GetProperties(BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.GetProperty)
    [ for namedField in fieldsSpec ->
        let matchedProp = Array.Find(properties, fun pi -> pi.Name = namedField.name)
        let propValue = matchedProp.GetValue value
        namedField, propValue ]

let rec serialize (valueSpec: ValueSpec) (value: obj) : Value =
    assertTypeMatched value valueSpec
    match valueSpec with
    | ValueSpec.Unit -> Value.Unit
    | ValueSpec.Integer -> Value.Integer (value :?> int)
    | ValueSpec.Float -> Value.Float (value :?> float)
    | ValueSpec.String -> Value.String (value :?> string)
    | ValueSpec.Bool -> Value.Bool (value :?> bool)
    | ValueSpec.List listItemSpec -> 
        let list = value :?> System.Collections.IEnumerable
        let rec serializeItems (enumerator: Collections.IEnumerator) =
            if enumerator.MoveNext() then
                let item = serialize listItemSpec (enumerator.Current)
                let items = serializeItems enumerator
                item :: items
            else []

        let items = serializeItems (list.GetEnumerator())
        Value.List(listItemSpec, items)
    | ValueSpec.Complex(typeName, namedSpec) -> 
        let t = value.GetType()
        let rec serializeFields (fields: (Named * obj) list) =
            match fields with
            | [] -> []
            | (named, value) :: xs -> 
                let item = serialize named.valueType value
                let item = { name = named.name; value = item }
                let items = serializeFields xs
                item :: items
        let fields = serializeFields (iterateFields namedSpec value)

        Value.Complex(typeName, fields)

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
