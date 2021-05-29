module Tests.Serialization

open System
open Xunit
open FSharp.Json
open RpcTypeExporter.ApiSpecification
open RpcTypeExporter.ValueSerialization

let mutable ctx = { complexTypeMap = Map.empty }

let serialize spec value =
    let serialized = serialize spec value
    serialized

let deserialize = deserialize ctx


[<Fact>]
let ``Integer serialized`` () =
    let value = 1
    let spec = ValueSpec.Integer
    let serializedValue = serialize spec value
    Assert.Equal(Value.Integer(1), serializedValue)

[<Fact>]
let ``String serialized`` () =    
    let value = "123"
    let spec = ValueSpec.String
    let serializedValue = serialize spec value
    Assert.Equal(Value.String("123"), serializedValue)
    
[<Fact>]
let ``Unit serialized`` () =    
    let value = ()
    let spec = ValueSpec.Unit
    let serializedValue = serialize spec value
    Assert.Equal(Value.Unit, serializedValue)

type SampleRecord = { a: int; b: bool }
let sampleRecordSpec = ValueSpec.Complex("SampleRecord", [
    {name = "a"; valueType = ValueSpec.Integer}
    {name = "b"; valueType = ValueSpec.Bool}])

[<Fact>]
let ``Sample record serialized`` () =    
    let value = { a = 0; b = false }

    let serializedValue = serialize sampleRecordSpec value

    let expectedValue = Value.Complex("SampleRecord", [
        { name = "a"; value = Value.Integer(0) }
        { name = "b"; value = Value.Bool(false) }])
    Assert.Equal(expectedValue, serializedValue)

[<Fact>]
let ``Sample list serialized`` () =    
    let value = [ 1; 2; 3 ]
    let spec = ValueSpec.List(ValueSpec.Integer)

    let serializedValue = serialize spec value

    let expectedValue = 
        Value.List(ValueSpec.Integer, 
                   [ Value.Integer(1); Value.Integer(2); Value.Integer(3) ])
    Assert.Equal(expectedValue, serializedValue)

type NestedRecordWithList = 
  { str: string
    unit: unit
    record: SampleRecord 
    list: int list}
let nestedRecordWithListSpec = ValueSpec.Complex("NestedRecordWithList", [
    { name = "str"; valueType = ValueSpec.String }
    { name = "unit"; valueType = ValueSpec.Unit }
    { name = "record"; valueType = sampleRecordSpec }
    { name = "list"; valueType = ValueSpec.List(ValueSpec.Integer) }])

[<Fact>]
let ``Nested record with list serialized`` () = 
    let value = { str = "s"; unit = (); record = { a = 1; b = false }; list = [ 0 ] }
    let serializedValue = serialize nestedRecordWithListSpec value
    
    let expectedValue = Value.Complex("NestedRecordWithList", [
        { name = "str"; value = Value.String("s") }
        { name = "unit"; value = Value.Unit }
        { name = "record"; value = Value.Complex("SampleRecord", [
            { name = "a"; value = Value.Integer(1) }
            { name = "b"; value = Value.Bool(false) } ])}
        { name = "list"; value = Value.List(ValueSpec.Integer, [ Value.Integer(0) ]) } ] )
    Assert.Equal(expectedValue, serializedValue)

[<Fact>]
let ``Deserialized value from json matches source`` () =
    let value = { str = "s"; unit = (); record = { a = 1; b = false }; list = [ 0 ] }
    let serializedValue = serialize nestedRecordWithListSpec value
    let json = Json.serialize serializedValue
    let deserializedFromJson = Json.deserialize<Value> json
    Assert.Equal(serializedValue, deserializedFromJson)

[<Fact>]
let ``Primitive type values deserialized to the same type`` () =
    Assert.Equal(0, deserialize (serialize ValueSpec.Integer 0) :?> int)
    Assert.Equal("123", deserialize (serialize ValueSpec.String "123") :?> string)
    
[<Fact>]
let ``List value deserialized to the same type`` () =
    let source = [1; 2; 3]
    let deserialized = deserialize (serialize (ValueSpec.List(ValueSpec.Integer)) source) :?> list<int>
    Assert.Equal<int>(source, deserialized)

[<Fact>]
let ``Record value deserialized to the same type`` () =
    let source = { a = 1; b = true }
    let _, ctx = RpcTypeExporter.ApiSpecification.getSpec SerializationContext.Empty (source.GetType())
    let serialized = RpcTypeExporter.ValueSerialization.serialize sampleRecordSpec source
    let deserialized = (RpcTypeExporter.ValueSerialization.deserialize ctx serialized) :?> SampleRecord
    Assert.Equal(source, deserialized)

[<Fact>]
let ``Debug test`` () =
    let value = { str = "s"; unit = (); record = { a = 1; b = false }; list = [ 0 ] }
    let serializedValue = serialize nestedRecordWithListSpec value
    let json = Json.serialize serializedValue
    let deserializedFromJson = Json.deserialize<Value> json
    Assert.True(true)
