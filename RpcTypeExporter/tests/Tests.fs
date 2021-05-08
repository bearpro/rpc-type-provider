module Tests

open System
open Xunit
open RpcTypeExporter

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
    let serilizedSpec = SpecificationSerializer.serilizeApiSpec<ISampleApi>()
    Assert.Equal("SampleApi", serilizedSpec.name)

[<Fact>]
let ``Sample api Sum() and Mul() members serilized right`` () =
    let serilizedSpec = SpecificationSerializer.serilizeApiSpec<ISampleApi>()
    let methods = serilizedSpec.methods
    
    let parameters = Complex([
        { name = "a"; valueType = Integer }
        { name = "b"; valueType = Integer } ])
    let retuns = Value.Integer

    Assert.Contains({ name = "Sum"; parameters = parameters; returns = retuns }, methods)
    Assert.Contains({ name = "Mul"; parameters = parameters; returns = retuns }, methods)

[<Fact>] 
let ``Simple record type serializedCorrect``() =
    let serilized = SpecificationSerializer.getTypeValue(typeof<DevisionArguments>)

    let expected = Complex([
        { name = "devisor"; valueType = Float }
        { name = "devisable"; valueType = Float }])

    Assert.Equal(expected, serilized)

[<Fact>]
let ``Sample api Devide() member serilized right`` () =
    let serilizedSpec = SpecificationSerializer.serilizeApiSpec<ISampleApi>()
    let methods = serilizedSpec.methods

    let parameters = Complex([
        { name = "args"; 
          valueType = Complex([
              { name = "devisor"; valueType = Float }
              { name = "devisable"; valueType = Float }])}])
    let returns = Complex([ { name = "result"; valueType = Float }])

    Assert.Contains({ name = "Devide"; parameters = parameters; returns = returns }, methods)

[<Fact>]
let ``List serialized correctly`` () =
    let serializedType = SpecificationSerializer.getTypeValue(typeof<int list>)
    Assert.Equal(List Integer, serializedType)

[<Fact>]
let ``Sample api DevideMany() member serialized`` () =
    let serilizedSpec = SpecificationSerializer.serilizeApiSpec<ISampleApi>()
    let methods = serilizedSpec.methods
    let parameters = Complex([
        { name = "argList"; 
          valueType = List(Complex([
              { name = "devisor"; valueType = Float }
              { name = "devisable"; valueType = Float }]))}])
    let returns = List(Complex([ { name = "result"; valueType = Float }]))
    let devideManySpec = { name = "DevideMany"; parameters = parameters; returns = returns }
    
    Assert.Contains(devideManySpec, methods)

[<Fact>]
let ``Primitive types serialized`` () =
    Assert.Equal(Value.Integer, SpecificationSerializer.getTypeValue(typeof<int>))
    Assert.Equal(Value.Float, SpecificationSerializer.getTypeValue(typeof<float>))
    Assert.Equal(Value.String, SpecificationSerializer.getTypeValue(typeof<string>))
    Assert.Equal(Value.Bool, SpecificationSerializer.getTypeValue(typeof<bool>))
    Assert.Equal(Value.Unit, SpecificationSerializer.getTypeValue(typeof<unit>))

[<Fact>]
let ``Some unsupported types not serialized`` () =    
    Assert.Throws<exn>(fun () -> 
        SpecificationSerializer.getTypeValue(
            typeof<System.Collections.Generic.Dictionary<int, int>>) 
        |> ignore)