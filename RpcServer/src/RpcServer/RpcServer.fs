module XRpcServer

open Giraffe
open RpcTypeExporter
open ApiSpecification.SpecificationSerializer

let app<'api, 'impl> (apiImplementation: 'impl) =
    let apiSpec = serializeApiSpec<'api>()
    choose []
    ()