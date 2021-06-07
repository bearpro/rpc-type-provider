module RpcTypeProviderImplementation

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open FSharp.Quotations
open FSharp.Core.CompilerServices
open MyNamespace
open ProviderImplementation
open ProviderImplementation.ProvidedTypes

// Put any utility helpers here
[<AutoOpen>]
module internal Helpers =
    let x = 1

[<TypeProvider>]
type BasicGenerativeProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config, assemblyReplacementMap=[("RpcTypeProvider.DesignTime", "RpcTypeProvider.Runtime")])

    let ns = "Sample"
    let asm = Assembly.GetExecutingAssembly()

    // check we contain a copy of runtime files, and are not referencing the runtime DLL
    do assert (typeof<DataSource>.Assembly.GetName().Name = asm.GetName().Name)  

    let createType typeName (count:int) =
        let asm = ProvidedAssembly()
        let myType = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, isErased=false)

        let ctor = ProvidedConstructor([], invokeCode = fun args -> <@@ "My internal state" :> obj @@>)
        myType.AddMember(ctor)

        let ctor2 = ProvidedConstructor([ProvidedParameter("InnerState", typeof<string>)], invokeCode = fun args -> <@@ (%%(args.[1]):string) :> obj @@>)
        myType.AddMember(ctor2)

        for i in 1 .. count do 
            let prop = ProvidedProperty("Property" + string i, typeof<int>, getterCode = fun args -> <@@ i @@>)
            myType.AddMember(prop)

        let meth = ProvidedMethod("StaticMethod", [], typeof<DataSource>, isStatic=true, invokeCode = (fun args -> Expr.Value(null, typeof<DataSource>)))
        myType.AddMember(meth)
        asm.AddTypes [ myType ]

        myType

    let myParamType = 
        let t = ProvidedTypeDefinition(asm, ns, "GenerativeProvider", Some typeof<obj>, isErased=false)
        t.DefineStaticParameters( [ProvidedStaticParameter("Count", typeof<int>)], fun typeName args -> createType typeName (unbox<int> args.[0]))
        t
    do
        this.AddNamespace(ns, [myParamType])

open RpcTypeExporter.ApiSpecification
open FSharp.Json
open System.Net.Http
open RpcTypeExporter.ValueSerialization
open System.Text

type TDef = ProvidedTypeDefinition
//type Dict = Dictionary<string, obj>

[<TypeProvider>]
type RpcTypeProvider(config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (
        config, 
        assemblyReplacementMap=[("RpcTypeProvider.DesignTime", "RpcTypeProvider.Runtime")])

    let ns = "RpcTypeProvider"
    let asm = Assembly.GetExecutingAssembly()
    let pasm = ProvidedAssembly()

    do assert (typeof<DataSource>.Assembly.GetName().Name = asm.GetName().Name)  

    let getApiSpec path : ApiSpec = 
        try 
            path |> (File.ReadAllText >> Json.deserialize)
        with e -> 
            let client = new HttpClient()
            let response = client.GetAsync(path).Result
            let responseJson = response.Content.ReadAsStringAsync().Result
            Json.deserialize responseJson

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

    let rpcMethodBody 
        (apiEndpoint: string) 
        (types: Type list)
        (httpClient: unit -> HttpClient) 
        (spec: MethodSpec) 
        (args: Expr list) =
        let inline castToObj (expr) = <@ %%expr :> obj @>
        match spec.parameters with
        | ValueSpec.Complex(b, parameters) -> 
            //failwithf "%A\n%A" parameters args
            let requestFieldsExpr = 
                //let inline serialize a b = serialize a b
                let serializeParamExpr (paramSpec: Named) (value: Expr) =
                    try
                        //let x = <@ { name = paramSpec.name; value = serialize (paramSpec.valueType) null } @>
                        //raise (ObjectException x)
                        let value' = Expr.Coerce(value, typeof<obj>)
                        <@ { name = paramSpec.name; value = serialize (paramSpec.valueType) %%value' } @>
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

    let rec getTypeOfSpec tdefs (spec: ValueSpec) : Type * TDef list =
        match spec with
        | ValueSpec.Unit -> typeof<unit>, tdefs
        | ValueSpec.Bool -> typeof<bool>, tdefs
        | ValueSpec.Integer -> typeof<int>, tdefs
        | ValueSpec.Float -> typeof<float>, tdefs
        | ValueSpec.String -> typeof<string>, tdefs
        | ValueSpec.List valueSpec -> 
            let itemType, types' = getTypeOfSpec tdefs valueSpec
            let listType = typedefof<list<_>>.MakeGenericType itemType
            listType, types'
        | ValueSpec.Complex(typeName, fields) -> 
            match List.tryFind (fun (t: TDef) -> t.Name = typeName) tdefs with 
            | Some tdef -> tdef.AsType(), tdefs
            | None ->
                let folder (types, typeDefs) (property: Named) =
                    let propertyType, types' = getTypeOfSpec typeDefs property.valueType
                    (property.name, propertyType) :: types, types'
                let fields, tdefs' = 
                    fields
                    |> List.rev
                    |> List.fold folder ([], tdefs) 

                let fieldName = sprintf "_%s"

                let providedFields = fields |> List.map (fun (n, t) -> ProvidedField(fieldName n, t))

                let providedProperty (n, t) field = 
                    ProvidedProperty(n, t, 
                        getterCode = (fun [this] -> Expr.FieldGet(this, field)),
                        setterCode = (fun [this; value] -> Expr.FieldSet(this, field, value)))
                let providedProperties = List.map2 providedProperty fields providedFields

                //let ctorArgs = List.map (fun (n, t) -> ProvidedParameter(n, t)) fields
                //let ctorBody (this :: args) : Expr =
                //    System.Diagnostics.Debug.Write(args)
                //    let args' = List.mapi (fun i x -> i,x) args
                //    let argExprs = [ 
                //        for i, value in args' ->
                //            let field = providedFields.[i]
                //            Expr.FieldSet(this, field, value) ]
                //    argExprs |> List.reduce (fun a b -> 
                //        <@@ 
                //            let _ = %%a 
                //            let _ = %%b
                //            () @@>)
                //let ctor = ProvidedConstructor(ctorArgs, invokeCode = ctorBody)
                
                let tdef = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, isErased = false)
                tdef.AddMembers providedFields
                tdef.AddMember(ProvidedConstructor([], fun _ -> <@@ () @@>))
                //tdef.AddMember ctor
                tdef.AddMembers providedProperties
                tdef.AsType(), tdef :: tdefs'

    let createParameters types methodName (valueSpec: ValueSpec) 
      : ProvidedParameter list * TDef list =
        let paramsTypeName = sprintf "%s.params" methodName
        match valueSpec with
        | ValueSpec.Complex (name, fields) when name = paramsTypeName -> 
            let folder (parameters, types) (parameterSpec: Named) =
                let paramType, types' = getTypeOfSpec types parameterSpec.valueType
                ProvidedParameter(parameterSpec.name, paramType) :: parameters, types'
            List.fold folder ([], types) fields
        | _ -> failwith "Invalid parameter declaration."

    let createRetrunType types (valueSpec: ValueSpec) =
        match valueSpec with
        | ValueSpec.Unit -> null, types
        | _ -> getTypeOfSpec types valueSpec

    let createMethod apiPath types (method: MethodSpec) =
        let parameters, types' = createParameters types method.name method.parameters
        let retrunType, types'' = createRetrunType types' method.returns
        let netTypes = types'' |> List.map (fun tdef -> tdef.AsType())
        let body = rpcMethodBody apiPath netTypes (fun () -> new HttpClient()) method
        //let body = fun _ -> <@@ () @@>
        ProvidedMethod(method.name, parameters, retrunType, invokeCode = body, isStatic = true), types''

    let createMethods apiPath types methods =
        let folder (methoDefs, types) method = 
            let methodDef, types' = createMethod apiPath types method
            methodDef :: methoDefs, types'
        List.fold folder ([], types) methods

    let createProvidedType typeName path =
        let spec = getApiSpec path

        let methods, types = createMethods path [] spec.methods

        let typeDef = ProvidedTypeDefinition(pasm, ns, typeName, Some typeof<obj>, isErased=false)
        typeDef.AddMembers(types)
        typeDef.AddMembers(methods)
        pasm.AddTypes([ typeDef ])
        typeDef
    
    let typeProviderType =
        let typeDef = ProvidedTypeDefinition(asm, ns, "RpcTypeProvider", Some typeof<obj>, isErased=false)
        typeDef.DefineStaticParameters(
            [ProvidedStaticParameter("SpecLocation", typeof<string>)], 
            fun typename args -> createProvidedType typename (args.[0] :?> string)
        )
        typeDef
    
    do
        this.AddNamespace(ns, [ typeProviderType ])

//[<TypeProvider>]
//type DtoTypeProvider(config : TypeProviderConfig) as this =
//    inherit TypeProviderForNamespaces (
//        config, 
//        assemblyReplacementMap=[("RpcTypeProvider.DesignTime", "RpcTypeProvider.Runtime")])
//    let ns = "RpcTypeProvider"
//    let asm = Assembly.GetExecutingAssembly()
//    let pasm = ProvidedAssembly()
//    do assert (typeof<DataSource>.Assembly.GetName().Name = asm.GetName().Name)  
//    let createProvidedType typeName =
//        let typeDef = ProvidedTypeDefinition(pasm, ns, typeName, Some typeof<obj>, isErased=false)
//        typeDef.AddMember(ProvidedConstructor([], fun args -> <@@ Dict() :> obj @@>))
//        typeDef.AddMember(
//            ProvidedProperty(
//                "IntValue", 
//                typeof<int>,
//                getterCode = (fun args -> <@@ ((%%(args.[0]) :> obj) :?> Dict).["IntValue"] :?> int @@>),
//                setterCode = (fun args -> <@@ ((%%(args.[0]) :> obj) :?> Dict).["IntValue"] <- %%args.[1] @@>)))
//        pasm.AddTypes([ typeDef ])
//        typeDef
//    let typeProviderType =
//        let typeDef = ProvidedTypeDefinition(asm, ns, "DtoTypeProvider", Some typeof<obj>)
//        typeDef.DefineStaticParameters([], fun typename args -> createProvidedType typename)
//        typeDef
//    do
//        this.AddNamespace(ns, [ typeProviderType ])

[<TypeProviderAssembly>]
do ()
