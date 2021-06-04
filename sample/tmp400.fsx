#r "nuget: FSharp.Json"

open FSharp.Json
open System.IO

type ValueSpec =
  | Unit
  | Integer
  | Float
  | String
  | Bool
  | List of valueType: ValueSpec
  | Complex of typeName: string * fields: Named list
and Named = { name: string; valueType: ValueSpec }

type MethodSpec = 
  { name: string 
    returns: ValueSpec
    parameters: ValueSpec }

type ApiSpec = 
  { name: string
    methods: MethodSpec list }

let config = JsonConfig.create(enumValue = EnumMode.Name)

// let path = @"C:\Users\mprokazin\source\repos\rpc-type-provider\sample\SimpleSample.spec.json"
// let json = File.ReadAllText path

// let result = Json.deserializeEx<ApiSpec> config json

[<JsonUnion(Mode = UnionMode.CaseKeyAsFieldName)>]
type SampleEnum = A | B | C of int

type SamplePayload = {
  a: SampleEnum
  b: SampleEnum
  c: SampleEnum
}

let v = { a = A; b = B; c = C(1)}

let s = Json.serializeEx (JsonConfig.Default)
s v |> printfn "%A"

printfn "OK!"