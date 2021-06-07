// #r "..\\RpcTypeExporter\\src\\RpcTypeExporter\\bin\\Debug\\netstandard2.0\\RpcTypeExporter.dll"
#r "..\\RpcTypeProvider\\src\\RpcTypeProvider.Runtime\\bin\\Debug\\typeproviders\\fsharp41\\netcoreapp3.1\\RpcTypeProvider.dll"
#r "nuget: FSharp.Json"

open RpcTypeExporter.ApiSpecification
open RpcTypeExporter.ValueSerialization
open System.Text
open FSharp.Json
open System.Net.Http

let c = new HttpClient()
c.GetAsync("http://localhost:5000/SampleApi") |> Async.AwaitTask |> Async.RunSynchronously

type Rpc = class
    static member Sum(a: int, b: int) = 
        async {
            use http = new HttpClient()
            let requestParam = Value.Complex("Sum.params", [
                { name = "a"; value = Integer(a) }
                { name = "b"; value = Integer(b) }
            ])
            let jsonPayload = Json.serialize requestParam
            let requestContent = new StringContent(jsonPayload, Encoding.UTF8, "application/json")
            let! resp = http.PostAsync("http://localhost:5000/SampleApi/Sum", requestContent) |> Async.AwaitTask
            let! responseJson = resp.Content.ReadAsStringAsync() |> Async.AwaitTask
            let value = Json.deserialize<Value> responseJson
            return 
                match value with
                | Value.Integer n -> n
                | _ -> failwith "INVALID RESPONSE"
        } |> Async.RunSynchronously

    static member Mul(a: int, b: int) = 
        async {
            use http = new HttpClient()
            let requestParam = Value.Complex("Mul.params", [
                { name = "a"; value = Integer(a) }
                { name = "b"; value = Integer(b) }
            ])
            let jsonPayload = Json.serialize requestParam
            let requestContent = new StringContent(jsonPayload, Encoding.UTF8, "application/json")
            let! resp = http.PostAsync("http://localhost:5000/SampleApi/Mul", requestContent) |> Async.AwaitTask
            let! responseJson = resp.Content.ReadAsStringAsync() |> Async.AwaitTask
            let value = Json.deserialize<Value> responseJson
            return 
                match value with
                | Value.Integer n -> n
                | _ -> failwith "INVALID RESPONSE"
        } |> Async.RunSynchronously
    end