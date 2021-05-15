module XRpcServer

open Giraffe
open RpcTypeExporter
open RpcTypeExporter.ApiSpecification
open RpcTypeExporter.ApiSpecification.SpecificationSerializer

module MemberCallDispatcher =
    open System
    open System.Reflection
    open FSharp.Json

    let parseMethodParams() =
        //FSharp.Json.
        ()

    let dispatchCall api (calledMethod: MethodSpec) callParams =
        let apiType = api.GetType()
        let result = 
            apiType.InvokeMember(
                calledMethod.name, 
                BindingFlags.Public ||| BindingFlags.Instance,
                Type.DefaultBinder,
                api,
                null)
        ()

let memberEndpoint (apiMethod: MethodSpec) = 
    subRoute apiMethod.name (text "kek")

let apiEntryPoint<'api> (apiImplementation: 'api) =
    try
        let apiSpec = serializeApiSpec<'api>()
        let members = 
            apiSpec.methods
            |> List.map memberEndpoint
        let apiEntryPoint = subRoute apiSpec.name (choose [
            POST >=> choose members
            GET >=> setStatusCode 400 >=> text "RPC only allows POST requests"
            ])
        apiEntryPoint
    with e -> failwith e.Message