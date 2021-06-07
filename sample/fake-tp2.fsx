//#r "..\\RpcTypeExporter\\src\\RpcTypeExporter\\RpcTypeExporter.fsproj"
//    RpcTypeExporter.fsproj
#r @"..\RpcTypeExporter\src\RpcTypeExporter\bin\Debug\netstandard2.0\RpcTypeExporter.dll"
#r "nuget: FSharp.Json"

open RpcTypeExporter.ApiSpecification
open RpcTypeExporter.ValueSerialization
open System.Text
open FSharp.Json
open System.Net.Http

let c = new HttpClient()
c.GetAsync("http://localhost:5000/SampleApi") |> Async.AwaitTask |> Async.RunSynchronously

module Rpc =
    type Book(Author: string, Year: int, Title: string) =
        do ()
        with
            member this.Author = Author
            member this.Year = Year
            member this.Title = Title

type Rpc =
    class
        static member Sum(a: int, b: int) =
            async {
                use http = new HttpClient()

                let requestParam =
                    Value.Complex(
                        "Sum.params",
                        [ { name = "a"; value = Integer(a) }
                          { name = "b"; value = Integer(b) } ]
                    )

                let jsonPayload = Json.serialize requestParam

                let requestContent =
                    new StringContent(jsonPayload, Encoding.UTF8, "application/json")

                let! resp =
                    http.PostAsync("http://localhost:5000/SampleApi/Sum", requestContent)
                    |> Async.AwaitTask

                let! responseJson =
                    resp.Content.ReadAsStringAsync()
                    |> Async.AwaitTask

                let value = Json.deserialize<Value> responseJson

                return
                    match value with
                    | Value.Integer n -> n
                    | _ -> failwith "INVALID RESPONSE"
            }
            |> Async.RunSynchronously

        static member Mul(a: int, b: int) =
            async {
                use http = new HttpClient()

                let requestParam =
                    Value.Complex(
                        "Mul.params",
                        [ { name = "a"; value = Integer(a) }
                          { name = "b"; value = Integer(b) } ]
                    )

                let jsonPayload = Json.serialize requestParam

                let requestContent =
                    new StringContent(jsonPayload, Encoding.UTF8, "application/json")

                let! resp =
                    http.PostAsync("http://localhost:5000/SampleApi/Mul", requestContent)
                    |> Async.AwaitTask

                let! responseJson =
                    resp.Content.ReadAsStringAsync()
                    |> Async.AwaitTask

                let value = Json.deserialize<Value> responseJson

                return
                    match value with
                    | Value.Integer n -> n
                    | _ -> failwith "INVALID RESPONSE"
            }
            |> Async.RunSynchronously

        static member FormatBibliographyEntry(book: Rpc.Book) =
            async {
                use http = new HttpClient()

                let requestParam =
                    Value.Complex(
                        "FormatBibliographyEntry.params",
                        [ { name = "book"
                            value =
                                Complex(
                                    "Book",
                                    ([ { name = "Author"
                                         value = Value.String(book.Author) }
                                       { name = "Year"
                                         value = Value.Integer(book.Year) }
                                       { name = "Title"
                                         value = Value.String(book.Title) } ])
                                ) } ]
                    )

                let jsonPayload = Json.serialize requestParam

                let requestContent =
                    new StringContent(jsonPayload, Encoding.UTF8, "application/json")

                let! resp =
                    http.PostAsync("http://localhost:5000/SampleApi/Mul", requestContent)
                    |> Async.AwaitTask

                let! responseJson =
                    resp.Content.ReadAsStringAsync()
                    |> Async.AwaitTask

                let value = Json.deserialize<Value> responseJson

                return
                    match value with
                    | String s -> s
                    | _ -> failwith "INVALID RESPONSE"
            }
            |> Async.RunSynchronously

        static member FormatBibliography(books: Rpc.Book list) =
            async {
                use http = new HttpClient()

                let bookSpec =
                    ValueSpec.Complex(
                        "Book",
                        [ { name = "Author"
                            valueType = ValueSpec.String }
                          { name = "PublicationYear"
                            valueType = ValueSpec.String }
                          { name = "Publisher"
                            valueType = ValueSpec.String }
                          { name = "Title"
                            valueType = ValueSpec.String }
                          { name = "City"
                            valueType = ValueSpec.String } ]
                    )

                let requestParam =
                    Value.Complex(
                        "FormatBibliography.params",
                        [ { name = "values"
                            value =
                                Value.List(
                                    bookSpec,
                                    (books
                                     |> List.map
                                         (fun book ->
                                             Value.Complex(
                                                 "FormatBibliographyEntry.params",
                                                 [ { name = "book"
                                                     value =
                                                         Complex(
                                                             "Book",
                                                             ([ { name = "Author"
                                                                  value = Value.String(book.Author) }
                                                                { name = "Year"
                                                                  value = Value.Integer(book.Year) }
                                                                { name = "Title"
                                                                  value = Value.String(book.Title) } ])
                                                         ) } ]
                                             )))
                                ) } ]
                    )

                let jsonPayload = Json.serialize requestParam

                let requestContent =
                    new StringContent(jsonPayload, Encoding.UTF8, "application/json")

                let! resp =
                    http.PostAsync("http://localhost:5000/SampleApi/Mul", requestContent)
                    |> Async.AwaitTask

                let! responseJson =
                    resp.Content.ReadAsStringAsync()
                    |> Async.AwaitTask

                let value = Json.deserialize<Value> responseJson

                return
                    match value with
                    | Value.Integer n -> n
                    | _ -> failwith "INVALID RESPONSE"
            }
            |> Async.RunSynchronously



    end
