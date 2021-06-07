module RpcServer.Server
open Giraffe
open System
open System.Reflection
open FSharp.Json
open Microsoft.AspNetCore.Http
open System.IO
open FSharp.Control.Tasks
open RpcTypeExporter
open RpcTypeExporter.ApiSpecification
open RpcServer.MemberCallDispatcher

let inline private json v = 
    let json = FSharp.Json.Json.serialize v
    text json

let methodCallHandler 
    (api: 'api) 
    (serializationContext: SerializationContext)
    (method: MethodSpec)
    : HttpHandler =
    fun (next: HttpFunc) (httpContext: HttpContext) -> 
        task {
            use reader = new StreamReader(httpContext.Request.Body) 
            let! requestJson = reader.ReadToEndAsync()
            let serializedParameters = Json.deserialize requestJson
            let parameters = parseMethodParams serializationContext serializedParameters
            let returnedValue = dispatchCall api method parameters
            let serializedValue = ValueSerialization.serialize method.returns returnedValue
            let jsonValue = Json.serialize serializedValue
            return! text jsonValue next httpContext
        }

let memberEndpoint 
    (api: 'api)
    (serializationContext: SerializationContext)
    (apiMethod: MethodSpec) = 
    routeCix $"\/{apiMethod.name}($|\/$)" >=> (methodCallHandler api serializationContext apiMethod)

let apiEntryPoint<'api> (apiImplementation: 'api) =
    let apiSpec, ctx = serializeApiSpec<'api>()
    let members = apiSpec.methods |> List.map (memberEndpoint apiImplementation ctx)
    let apiEntryPoint = 
        subRoute $"/{apiSpec.name}" (
            choose [
                GET >=> setStatusCode 200 >=> json apiSpec
                POST >=> choose members
            ])
    apiEntryPoint


