namespace RpcTypeExporter.ApiSpecification

open System
open System.Reflection
open System.IO
open Newtonsoft.Json
open RpcTypeExporter.ApiSpecification

module ValueSerialization =
    type Value =
    | Unit
    | Integer of int
    | Float of float
    | String of string
    | Bool of bool
    | List of valueType: ValueSpec * Value list
    | Complex of NamedValue list
    and NamedValue = { name: string; value: Value }

    let assertTypeMatched (value: obj) valueSpec =
        let fail() = failwithf "Value %A not matches specification %A" value valueSpec
        if (value = null && valueSpec = ValueSpec.Unit) then
            ()
        else
            let actualValueSpec = SpecificationSerializer.getValueSpec(value.GetType())
            if actualValueSpec = valueSpec then () else fail()

    let iterateFields (fieldsSpec: Named list) (value: obj) =
        let t = value.GetType()
        let properties = t.GetProperties(BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.GetProperty)
        [ for namedField in fieldsSpec ->
            let matchedProp = Array.Find(properties, fun pi -> pi.Name = namedField.name)
            let propValue = matchedProp.GetValue value
            namedField, propValue ]

    let rec serialize (valueSpec: ValueSpec) (value: obj)  =
        assertTypeMatched value valueSpec
        match valueSpec with
        | ValueSpec.Unit -> Value.Unit
        | ValueSpec.Integer -> Value.Integer (value :?> int)
        | ValueSpec.Float -> Value.Float (value :?> float)
        | ValueSpec.String -> Value.String (value :?> string)
        | ValueSpec.Bool -> Value.Bool (value :?> bool)
        | ValueSpec.List listItemSpec -> 
            let enumerable = value :?> System.Collections.IEnumerable
            let items = [ for item in enumerable -> serialize listItemSpec item ]
            Value.List(listItemSpec, items)
        | ValueSpec.Complex namedSpec -> 
            let fields = 
                iterateFields namedSpec value 
                |> List.map (fun (s, v) -> { name = s.name; value = serialize s.valueType v })
            Value.Complex fields
            

