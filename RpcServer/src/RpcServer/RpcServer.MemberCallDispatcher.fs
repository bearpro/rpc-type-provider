module RpcServer.MemberCallDispatcher

open Giraffe
open RpcTypeExporter
open System
open System.Reflection
open FSharp.Json
open RpcTypeExporter.ApiSpecification
open RpcTypeExporter.ValueSerialization

let inline json v = 
    let json = FSharp.Json.Json.serialize v
    text json

let parseMethodParams ctx value : obj[] =
    let fail() = failwithf "Value %A not matches method params." value
    match value with
    | Complex (name, fields) ->
        if name.Contains("params") then
            [ for field in fields ->
                deserialize ctx field.value ]
            |> Array.ofList
        else fail()
    | Unit -> array.Empty()
    | _ -> fail()

let dispatchCall<'api> (api: 'api) (calledMethod: MethodSpec) callParams =
    let apiType = typeof<'api>
    let result = 
        apiType.InvokeMember(
            calledMethod.name, 
            BindingFlags.InvokeMethod ||| BindingFlags.Public ||| BindingFlags.Instance,
            Type.DefaultBinder,
            api,
            callParams)
    result
