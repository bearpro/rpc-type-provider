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

let apiSpec, ctx = RpcTypeExporter.ApiSpecification.serializeApiSpec<ISampleApi>()
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

[<Fact>]
let ``Method call dispatched right`` () =
    let value = 
        MemberCallDispatcher.parseMethodParams 
            ctx 
            (Value.Complex("Sum.params", [
                { name = "a"; value = Value.Integer(3) }
                { name = "b"; value = Value.Integer(4) }]))
    let methodSpec = apiSpec.methods |> Seq.find (fun x -> x.name = "Sum")
    let result = MemberCallDispatcher.dispatchCall api methodSpec value
    Assert.Equal(result :?> int, 7)

type IHasParameterlessMethod =
    abstract NoParamsMethod: unit -> int

type ClassWithParametrlessMethod() =
    interface IHasParameterlessMethod with
        member __.NoParamsMethod() = -1

[<Fact>]
let ``Parameterless method dispatched`` () =
    let apiInstance = ClassWithParametrlessMethod() :> IHasParameterlessMethod
    let apiSpec, ctx = serializeApiSpec<IHasParameterlessMethod>()
    let methodSpec = apiSpec.methods |> Seq.find (fun x -> x.name = "NoParamsMethod")
    let value = MemberCallDispatcher.parseMethodParams ctx Value.Unit
    let result = MemberCallDispatcher.dispatchCall apiInstance methodSpec value
    Assert.Equal(result :?> int, -1)