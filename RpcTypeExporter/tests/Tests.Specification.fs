module Tests.Specification

open System
open Xunit
open RpcTypeExporter.ApiSpecification

type DevisionArguments = 
  { devisor: float 
    devisable: float }

type DevisionResult =
  { result: float }

type ISampleApi =
    abstract Sum: a: int -> b: int -> int
    abstract Mul: a: int -> b: int -> int
    abstract Devide: args: DevisionArguments -> DevisionResult
    abstract DevideMany: argList: DevisionArguments list -> DevisionResult list

[<Fact>]
let ``Sample api spec name right`` () =
    let serilizedSpec = serializeApiSpec<ISampleApi>()
    Assert.Equal("SampleApi", serilizedSpec.name)

[<Fact>]
let ``Sample api Sum() and Mul() members serilized right`` () =
    let serilizedSpec = serializeApiSpec<ISampleApi>()
    let methods = serilizedSpec.methods
    
    let parameters m = Complex($"{m}.params", [
        { name = "a"; valueType = Integer }
        { name = "b"; valueType = Integer } ])
    let retuns = ValueSpec.Integer

    Assert.Contains({ name = "Sum"; parameters = parameters "Sum"; returns = retuns }, methods)
    Assert.Contains({ name = "Mul"; parameters = parameters "Mul"; returns = retuns }, methods)

[<Fact>] 
let ``Simple record type serializedCorrect``() =
    let serilized = getValueSpec(typeof<DevisionArguments>)

    let expected = Complex("DevisionArguments", [
        { name = "devisor"; valueType = Float }
        { name = "devisable"; valueType = Float }])

    Assert.Equal(expected, serilized)

[<Fact>]
let ``Sample api Devide() member serilized right`` () =
    let serilizedSpec = serializeApiSpec<ISampleApi>()
    let methods = serilizedSpec.methods

    let parameters = Complex("Devide.params", [
        { name = "args"; 
          valueType = Complex("DevisionArguments", [
              { name = "devisor"; valueType = Float }
              { name = "devisable"; valueType = Float }])}])
    let returns = Complex("DevisionResult", [ { name = "result"; valueType = Float }])

    Assert.Contains({ name = "Devide"; parameters = parameters; returns = returns }, methods)

[<Fact>]
let ``List serialized correctly`` () =
    let serializedType = getValueSpec(typeof<int list>)
    Assert.Equal(List Integer, serializedType)

[<Fact>]
let ``Sample api DevideMany() member serialized`` () =
    let serilizedSpec = serializeApiSpec<ISampleApi>()
    let methods = serilizedSpec.methods
    let parameters = Complex("DevideMany.params", [
        { name = "argList"; 
          valueType = List(Complex("DevisionArguments", [
              { name = "devisor"; valueType = Float }
              { name = "devisable"; valueType = Float }]))}])
    let returns = List(Complex("DevisionResult", [ { name = "result"; valueType = Float }]))
    let devideManySpec = { name = "DevideMany"; parameters = parameters; returns = returns }
    
    Assert.Contains(devideManySpec, methods)

[<Fact>]
let ``Primitive types serialized`` () =
    Assert.Equal(ValueSpec.Integer, getValueSpec(typeof<int>))
    Assert.Equal(ValueSpec.Float, getValueSpec(typeof<float>))
    Assert.Equal(ValueSpec.String, getValueSpec(typeof<string>))
    Assert.Equal(ValueSpec.Bool, getValueSpec(typeof<bool>))
    Assert.Equal(ValueSpec.Unit, getValueSpec(typeof<unit>))

[<Fact>]
let ``Some unsupported types not serialized`` () =    
    Assert.Throws<exn>(fun () -> 
        getValueSpec(typeof<System.Collections.Generic.Dictionary<int, int>>) 
        |> ignore)