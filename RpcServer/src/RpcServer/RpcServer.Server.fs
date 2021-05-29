module RpcServer.Server
open Giraffe
open RpcTypeExporter
open RpcTypeExporter.ApiSpecification
open System
open System.Reflection
open FSharp.Json

let inline json v = 
    let json = FSharp.Json.Json.serialize v
    text json

let memberEndpoint (apiMethod: MethodSpec) = 
    subRoute $"/{apiMethod.name}" (text "kek")

let apiEntryPoint<'api> (apiImplementation: 'api) =
    let apiSpec = serializeApiSpec<'api>()
    let members = apiSpec.methods |> List.map memberEndpoint
    let apiEntryPoint = 
        subRoute $"/{apiSpec.name}" (
            choose [
                GET >=> setStatusCode 200 >=> json apiSpec
                POST >=> choose members
            ])
    apiEntryPoint


