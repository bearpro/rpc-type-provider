module WebAppTests

open System
open Xunit
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.AspNetCore.TestHost;
open Giraffe
open Microsoft.Extensions.DependencyInjection
open System.Net
open System.Net.Http

type ISampleApi =
    abstract Sum: a: int -> b: int -> int
    abstract Mul: a: int -> b: int -> int

type SampleApi() =
    interface ISampleApi with
        member _.Sum a b = a + b
        member _.Mul a b = a * b

let webApp() = 
    let api = SampleApi() :> ISampleApi
    let apiEntryPoint = XRpcServer.apiEntryPoint api
    choose [ 
        apiEntryPoint
        setStatusCode 404 >=> text "Not Found" ]

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
let ``Sum request returns result`` () =
    let testBody (client: HttpClient) = async {
        let response = client.GetAsync("/").Result
        Assert.Equal(HttpStatusCode.OK, response.StatusCode)
    }
    withClient testBody
