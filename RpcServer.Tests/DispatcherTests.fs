module DispatcherTests

open System
open Xunit
open FSharp.Json
open RpcServer
open RpcTypeExporter.ApiSpecification
open RpcTypeExporter.ValueSerialization

type ISampleApi =
    abstract Sum: a: int -> b: int -> int
    abstract Mul: a: int -> b: int -> int

type SampleApi() =
    interface ISampleApi with
        member _.Sum a b = a + b
        member _.Mul a b = a * b

let mutable ctx = { complexTypeMap = Map.empty }

let apiSpec, _ = RpcTypeExporter.ApiSpecification.serializeApiSpec<ISampleApi>()
let api = SampleApi() :> ISampleApi

[<Fact>]
let ``Member called and returned value correct`` () =
    let methodSpec = apiSpec.methods |> Seq.find (fun x -> x.name = "Sum")
    let result = MemberCallDispatcher.dispatchCall api methodSpec ([|1; 1|])
    Assert.Equal(result :?> int, 2)

[<Fact>]
let ``Json params deserialized in correct array`` () =
    let value = 
        MemberCallDispatcher.parseMethodParams 
            ctx 
            (Value.Complex("Sum.params", [
                { name = "a"; value = Value.Integer(1) }
                { name = "b"; value = Value.Integer(2) }]))
    Assert.Equal(1, value.[0] :?> int)
    Assert.Equal(2, value.[1] :?> int)