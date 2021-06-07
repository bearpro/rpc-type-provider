namespace MyNamespace

open System
open RpcTypeExporter.ApiSpecification
open RpcTypeExporter.ValueSerialization
open Microsoft.FSharp.Quotations
open FSharp.Json
open System.Net.Http
open System.Text
open System.Reflection

type ObjectException(v:'a) = 
    inherit Exception("OBJECT")
    with 
        member _.Value = v

// Put any utilities here
[<AutoOpen>]
module internal Utilities = 
    let rec deserialize (types: Type list) (value: Value) : obj =
        match value with
        | Value.Unit -> () :> obj
        | Value.Integer x -> x :> obj
        | Value.Float x -> x :> obj
        | Value.String x -> x :> obj
        | Value.Bool x -> x :> obj
        | Value.List(listItemSpec, values) -> 
            let sourceList = values |> List.map (deserialize types) |> List.rev
            let mutableListType = typedefof<list<_>>
            let mutableListType' = mutableListType.MakeGenericType(sourceList.Head.GetType())
            let emptyProp = mutableListType'.GetProperty("Empty")
            let consMethod = mutableListType'.GetMethod("Cons")
            let mutable typedList = emptyProp.GetValue(null)
            for item in sourceList do
                typedList <- consMethod.Invoke(null, [| item; typedList |])
            typedList
        | Value.Complex(typeName, fields) -> 
            let deserializedFields = fields |> List.map (fun x -> deserialize types x.value) |> Array.ofList
            let deserializedFieldTypes = deserializedFields |> Array.map (fun x -> x.GetType())
            let t = types |> List.find (fun t -> t.Name = typeName)
            let ctor = t.GetConstructor(deserializedFieldTypes)
            let instance = ctor.Invoke(deserializedFields)
            let _ = Array.map2 (fun (prop: PropertyInfo) v -> prop.SetValue(instance, v) ) (t.GetProperties()) deserializedFields
            instance
    let mutable hc = 0;
    
    let rpcMethodBody 
        (apiEndpoint: string) 
        (types: Type list)
        (httpClient: unit -> HttpClient) 
        (spec: MethodSpec) 
        (args: Expr list) =
        match spec.parameters with
        | ValueSpec.Complex(b, parameters) -> 
            //failwithf "%A\n%A" parameters args
            let requestFieldsExpr = 
                let serializeParamExpr (paramSpec: Named) (value: Expr) =
                    try
                        //let x = <@ { name = paramSpec.name; value = serialize (paramSpec.valueType) null } @>
                        //raise (ObjectException x)
                        //let value' = Expr.Coerce(value, typeof<obj>)
                        <@ { name = paramSpec.name; value = serialize (paramSpec.valueType) %%value } @>
                    with e -> failwithf "LOL ERROR\n%s\n%s" e.Message e.StackTrace
                let paramExprs = List.map2 serializeParamExpr parameters args
                let state = <@ [] @> : Expr<NamedValue list>
                let x = List.foldBack (fun a (b: Expr<NamedValue list>) -> <@(%a) :: (%b)@>) paramExprs state 
                x
            let requestExpr = 
                <@@ 
                    use httpClient = httpClient()
                    let requestParam = Value.Complex(sprintf "%s.params" spec.name, %requestFieldsExpr) 
                    let jsonPayload = Json.serialize requestParam
                    let requestContent = new StringContent(jsonPayload, Encoding.UTF8, "application/json")
                    let url = sprintf "%s/%s" apiEndpoint spec.name
                    let response = httpClient.PostAsync(url, requestContent).Result
                    let responseJson = response.Content.ReadAsStringAsync().Result
                    let responseJson = Json.deserialize<Value> responseJson
                    let objResponse = deserialize types responseJson
                    objResponse
                @@>
            requestExpr
        | x -> failwithf "Invalid spec %A" x

// Put any runtime constructs here
type DataSource(filename:string) = 
    member this.FileName = filename


// Put the TypeProviderAssemblyAttribute in the runtime DLL, pointing to the design-time DLL
[<assembly:CompilerServices.TypeProviderAssembly("RpcTypeProvider.DesignTime.dll")>]
do ()
