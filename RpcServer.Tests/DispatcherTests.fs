module DispatcherTests

open System
open Xunit
open FSharp.Json

[<Fact>]
let ``Json payload deserialized`` () =
    let json = """{ "intValue": 0 }"""
    let conf = JsonConfig.create(allowUntyped = true)
    let value = FSharp.Json.Json.deserializeEx conf json
    Assert.True(true)
