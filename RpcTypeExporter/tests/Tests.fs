module Tests

open System
open Xunit
open RpcTypeExporter

type ISampleApi =
    abstract Sum: a: int -> b: int -> int
    abstract Mul: a: int -> b: int -> int

[<Fact>]
let ``Sample api spec name right`` () =
    let serilizedSpec = RpcTypeExporter.SpecificationSerializer.serilizeApiSpec<ISampleApi>()
    Assert.Equal("SampleApi", serilizedSpec.name)

[<Fact>]
let ``Sample api Sum() and Mul() members serilized right`` () =
    let serilizedSpec = RpcTypeExporter.SpecificationSerializer.serilizeApiSpec<ISampleApi>()
    let methods = serilizedSpec.methods
    let parameters = Complex([
        { name = "a"; valueType = Value.Integer }
        { name = "b"; valueType = Value.Integer } ])
    let retuns = Value.Integer
    Assert.Contains({ name = "Sum"; parameters = parameters; returns = retuns }, methods)
    Assert.Contains({ name = "Mul"; parameters = parameters; returns = retuns }, methods)
