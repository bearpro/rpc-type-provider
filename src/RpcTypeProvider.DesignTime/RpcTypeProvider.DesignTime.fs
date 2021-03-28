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
open FSharp.Json

open MyNamespace

[<AutoOpen>]
module internal Helpers =
    let rec getType paramType = 
        match paramType with
        | Unit -> typeof<unit>
        | Integer -> typeof<int>
        | Float -> typeof<float>
        | String -> typeof<string>
        | List t -> 
            let generic = typedefof<list<_>>
            generic.MakeGenericType([|getType t|])

    let providedMethodParam i paramType =
        ProvidedParameter($"p_%i{i}", getType paramType)

    let providedMethod apiMethod =
        let methodName = $"{apiMethod.HttpMethod}_{apiMethod.MethodName}"
        let parameterTypes = apiMethod.Params |> List.mapi providedMethodParam
        let returnType = getType apiMethod.Returns
        let errorMsg = $"{apiMethod.HttpMethod} {apiMethod.Endpoint}"
        let invokeCode = <@@failwith errorMsg @@>
        let method = ProvidedMethod(methodName, parameterTypes, returnType, fun args -> invokeCode)
        method

[<TypeProvider>]
type RpcTypeProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (
        config, 
        assemblyReplacementMap=[("RpcTypeProvider.DesignTime", "RpcTypeProvider.Runtime")])

    let ns = "RpcTypeProvider"
    let asm = Assembly.GetExecutingAssembly()

    do assert (typeof<DataSource>.Assembly.GetName().Name = asm.GetName().Name)  

    let createType typeName (apiDefinitionPath: string ) =
        let definitionJson = System.IO.File.ReadAllText(apiDefinitionPath)
        let apiDefinition: InterfaceMethod list = Json.deserialize definitionJson
        let asm = ProvidedAssembly()
        let myType = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, isErased=false)

        let ctor = ProvidedConstructor([], invokeCode = fun args -> <@@ "My internal state" :> obj @@>)
        myType.AddMember(ctor)

        for apiMethod in apiDefinition do 
            let method = providedMethod apiMethod
            myType.AddMember(method)

        asm.AddTypes [ myType ]
        myType

    let myParamType = 
        let t = ProvidedTypeDefinition(asm, ns, "RpcProvider", Some typeof<obj>, isErased=false)
        t.DefineStaticParameters( 
            [ProvidedStaticParameter("definitionPath", typeof<string>)], 
            fun typeName args -> createType typeName (args.[0] :?> string ))
        t
    do
        this.AddNamespace(ns, [myParamType])

[<TypeProviderAssembly>]
do ()

