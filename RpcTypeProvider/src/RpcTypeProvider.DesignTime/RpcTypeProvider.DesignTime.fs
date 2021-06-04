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

type TDef = ProvidedTypeDefinition

[<TypeProvider>]
type RpcTypeProvider(config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (
        config, 
        assemblyReplacementMap=[("RpcTypeProvider.DesignTime", "RpcTypeProvider.Runtime")])

    let ns = "RpcTypeProvider"
    let asm = Assembly.GetExecutingAssembly()
    let pasm = ProvidedAssembly()

    do assert (typeof<DataSource>.Assembly.GetName().Name = asm.GetName().Name)  

    let getApiSpec: string -> ApiSpec = File.ReadAllText >> Json.deserialize

    let rec getTypeOfSpec tdefs (spec: ValueSpec) : Type * TDef list =
        match spec with
        | Unit -> typeof<unit>, tdefs
        | Bool -> typeof<bool>, tdefs
        | Integer -> typeof<int>, tdefs
        | Float -> typeof<float>, tdefs
        | String -> typeof<string>, tdefs
        | List valueSpec -> 
            let itemType, types' = getTypeOfSpec tdefs valueSpec
            let listType = typedefof<list<_>>.MakeGenericType itemType
            listType, types'
        | Complex(typeName, fields) -> 
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
                
                let providedFields = 
                    fields 
                    |> List.map (fun (n, t) -> 
                        let name = sprintf "_%s" n
                        ProvidedField(name, t))

                let providedProperties = 
                    List.map2 
                        (fun (name, t) (field: ProvidedField) -> 
                            ProvidedProperty(name, t, 
                                getterCode = (
                                    fun args -> 
                                        <@@ 
                                            let this = %%args.[0]
                                            this.GetType().GetField(field.Name).GetValue(this)
                                        @@>)))
                        fields
                        providedFields

                let ctorArgs = List.map ProvidedParameter fields
                let ctorBody (args: Expr list) : Expr =
                    let this = args.[0]
                    let args' = (List.skip 1 >> List.mapi (fun i x -> i,x)) args
                    let argExprs = [ 
                        for i, arg in args' ->
                            let propName, _ = fields.[i]
                            let fieldName = sprintf "_%s" propName
                            <@@ (%%this).GetType().GetField(fieldName).SetValue(%%this, %%arg) @@> ]
                    let resultExpr = argExprs |> List.reduce (fun a b -> 
                        <@@ %%a 
                            %%b @@>)
                    resultExpr
                let ctor = ProvidedConstructor(ctorArgs, invokeCode = ctorBody)
                
                let tdef = ProvidedTypeDefinition(pasm, ns, typeName, Some(typeof<obj>), isErased = false)
                tdef.AddMembers providedFields
                tdef.AddMembersDelayed(fun () -> providedProperties)
                tdef.AddMemberDelayed(fun () -> ctor)
                tdef.AsType(), tdef :: tdefs'

    let createParameters types methodName (valueSpec: ValueSpec) 
      : ProvidedParameter list * TDef list =
        let paramsTypeName = sprintf "%s.params" methodName
        match valueSpec with
        | Complex (name, fields) when name = paramsTypeName -> 
            let folder (parameters, types) (parameterSpec: Named) =
                let paramType, types' = getTypeOfSpec types parameterSpec.valueType
                ProvidedParameter(parameterSpec.name, paramType) :: parameters, types'
            List.fold folder ([], types) fields
        | _ -> failwith "Invalid parameter declaration."

    let createRetrunType types (valueSpec: ValueSpec) =
        match valueSpec with
        | Unit -> null, types
        | _ -> getTypeOfSpec types valueSpec

    let createMethod types (method: MethodSpec) =
        let parameters, types' = createParameters types method.name method.parameters
        let retrunType, types'' = createRetrunType types' method.returns
        ProvidedMethod(method.name, parameters, retrunType, invokeCode = (fun args -> <@@ obj() @@>), isStatic = true), types''

    let createMethods types methods =
        let folder (methoDefs, types) method = 
            let methodDef, types' = createMethod types method
            methodDef :: methoDefs, types'
        List.fold folder ([], types) methods

    let createProvidedType typeName path =
        let spec = getApiSpec path

        let methods, types = createMethods [] spec.methods

        let typeDef = ProvidedTypeDefinition(pasm, ns, typeName, Some typeof<obj>, isErased=false)
        typeDef.AddMembers(methods)
        typeDef.AddMembers(types)
        pasm.AddTypes(typeDef :: types)
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

[<TypeProviderAssembly>]
do ()
