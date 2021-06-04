module ServerTests

open System
open Xunit
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.AspNetCore.TestHost;
open Giraffe
open Microsoft.Extensions.DependencyInjection
open System.Net
open System.Net.Http
open FSharp.Json

type Book = 
  { Author: string
    PublicationYear: int 
    Publisher: string
    Title: string
    City: string }

type ISampleApi =
    abstract Sum: a: int -> b: int -> int
    abstract Mul: a: int -> b: int -> int
    abstract FormatBibliographyEntry: book: Book -> string
    abstract SumAll: values: int list -> int
    abstract FormatBibliography: books: Book list -> string list

type SampleApi() =
    interface ISampleApi with
        member _.Sum a b = a + b
        member _.Mul a b = a * b
        member _.FormatBibliographyEntry book = 
            $"{book.Author} / {book.Title} / {book.City}, {book.Publisher} {book.PublicationYear}"
        member _.SumAll values = List.sum values
        member this.FormatBibliography books = 
            books
            |> List.map (this :> ISampleApi).FormatBibliographyEntry 

let webApp() = 
    let api = SampleApi() :> ISampleApi
    let apiEntryPoint = RpcServer.Server.apiEntryPoint api
    apiEntryPoint

open RpcServer
open RpcTypeExporter.ApiSpecification
open System.Text
open RpcTypeExporter.ValueSerialization

let (^) f x = f x
let webHost =
    WebHostBuilder()
        .Configure(fun app -> app.UseGiraffe ^ Server.apiEntryPoint ^ SampleApi())
        .ConfigureServices(fun services -> ignore ^ services.AddGiraffe())

let withClient (testFunction: HttpClient -> Async<unit>) =
    let host = 
        WebHostBuilder()
            .Configure(fun app -> app.UseGiraffe(webApp()))
            .ConfigureServices(fun services -> services.AddGiraffe() |> ignore)
    use server = new TestServer(host)
    let client = server.CreateClient()
    testFunction client |> Async.RunSynchronously

[<Fact>]
let ``Root request returns 404`` () =
    let testBody (client: HttpClient) = async {
        let response = client.GetAsync("/").Result
        Assert.Equal(HttpStatusCode.NotFound, response.StatusCode)
    }
    withClient testBody

[<Fact>]
let ``Api spec returned for GET request at api endpoint`` () =
    let testBody (client: HttpClient) = async {
        let resp = client.GetAsync("/SampleApi").Result
        Assert.Equal(HttpStatusCode.OK, resp.StatusCode)
        let json = resp.Content.ReadAsStringAsync().Result
        let spec = Json.deserialize<ApiSpec> json
        Assert.Equal("SampleApi", spec.name)
    }
    withClient testBody

[<Fact>]
let ``Call with primitive type parameters returns correct result`` () =
    let testBody (client: HttpClient) = async {
        let requestParam = Value.Complex("Sum.params", [
            { name = "a"; value = Integer(5) }
            { name = "b"; value = Integer(3) }
        ])
        let jsonPayload = Json.serialize requestParam
        let requestContent = new StringContent(jsonPayload, Encoding.UTF8, "application/json")
        let! response = client.PostAsync("/SampleApi/Sum", requestContent) |> Async.AwaitTask
        Assert.Equal(HttpStatusCode.OK, response.StatusCode)
        
        let! responseJson = response.Content.ReadAsStringAsync() |> Async.AwaitTask
        let response = Json.deserialize<Value> responseJson
        Assert.Equal(Integer(8), response)
    }
    withClient testBody

[<Fact>]
let ``Call with complex type parameter returns correct result`` () =
    let testBody (client: HttpClient) = async {
        let requestParam = Value.Complex("FormatBibliographyEntry.params", [{ 
            name = "book"; value = 
                Complex(
                    "Book", 
                    ([
                        { name = "Author"; value = Value.String("Вася П.")}
                        { name = "PublicationYear"; value = Value.Integer(2002)}
                        { name = "Publisher"; value = Value.String("Питер")}
                        { name = "Title"; value = Value.String("Самогонные аппараты")}
                        { name = "City"; value = Value.String("СПб")}
                    ])
                )
            }])
        let jsonPayload = Json.serialize requestParam
        let requestContent = new StringContent(jsonPayload, Encoding.UTF8, "application/json")
        let! response = 
            client.PostAsync("/SampleApi/FormatBibliographyEntry", requestContent) 
            |> Async.AwaitTask
        Assert.Equal(HttpStatusCode.OK, response.StatusCode)
        
        let! responseJson = response.Content.ReadAsStringAsync() |> Async.AwaitTask
        let response = Json.deserialize<Value> responseJson
        Assert.Equal(String($"Вася П. / Самогонные аппараты / СПб, Питер 2002"), response)
    }
    withClient testBody

[<Fact>]
let ``Call with list parameter returns correct result`` () =
    let testBody (client: HttpClient) = async {
        let requestParam = Value.Complex("SumAll.params", [{ 
            name = "values"; value = Value.List(ValueSpec.Integer, [
                Integer(1)
                Integer(1)
                Integer(1)])}])
        let jsonPayload = Json.serialize requestParam
        let requestContent = new StringContent(jsonPayload, Encoding.UTF8, "application/json")
        let! response = 
            client.PostAsync("/SampleApi/SumAll", requestContent) 
            |> Async.AwaitTask
        Assert.Equal(HttpStatusCode.OK, response.StatusCode)
        
        let! responseJson = response.Content.ReadAsStringAsync() |> Async.AwaitTask
        let response = Json.deserialize<Value> responseJson
        Assert.Equal(Integer(3), response)
    }
    withClient testBody

[<Fact>]
let ``Call with parameter of list of complex values returns correct result`` () =
    let testBody (client: HttpClient) = async {
        let bookSpec = ValueSpec.Complex("Book", [
            { name = "Author"; valueType = ValueSpec.String }
            { name = "PublicationYear"; valueType = ValueSpec.String }
            { name = "Publisher"; valueType = ValueSpec.String }
            { name = "Title"; valueType = ValueSpec.String }
            { name = "City"; valueType = ValueSpec.String }
        ])
        let requestParam = Value.Complex("FormatBibliography.params", [{ 
            name = "values"; value = Value.List(bookSpec, [
                Complex(
                    "Book", 
                    ([
                        { name = "Author"; value = Value.String("Вася П.")}
                        { name = "PublicationYear"; value = Value.Integer(2002)}
                        { name = "Publisher"; value = Value.String("Питер")}
                        { name = "Title"; value = Value.String("Самогонные аппараты")}
                        { name = "City"; value = Value.String("СПб")}
                    ])
                )])}])
        let jsonPayload = Json.serialize requestParam
        let requestContent = new StringContent(jsonPayload, Encoding.UTF8, "application/json")
        let! response = 
            client.PostAsync("/SampleApi/FormatBibliography", requestContent) 
            |> Async.AwaitTask
        Assert.Equal(HttpStatusCode.OK, response.StatusCode)
        
        let! responseJson = response.Content.ReadAsStringAsync() |> Async.AwaitTask
        let response = Json.deserialize<Value> responseJson
        let expected = List(ValueSpec.String, [ String($"Вася П. / Самогонные аппараты / СПб, Питер 2002") ])
        Assert.Equal(expected, response)
    }
    withClient testBody