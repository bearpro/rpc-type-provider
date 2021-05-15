namespace RpcTypeExporter.ApiSpecification

open System
open System.Reflection

type ValueSpec =
  | Unit
  | Integer
  | Float
  | String
  | Bool
  | List of valueType: ValueSpec
  | Complex of Named list
and Named = { name: string; valueType: ValueSpec }

type MethodSpec = 
  { name: string 
    returns: ValueSpec
    parameters: ValueSpec }

type ApiSpec = 
  { name: string
    methods: MethodSpec list }

module SpecificationSerializer =

    let publicInstance = BindingFlags.Public ||| BindingFlags.Instance

    let getApiName (apiType: Type) =
        let typeName = apiType.Name
        if typeName.[0] = 'I' then
            typeName.[1..]
        else
            typeName

    let rec getValueSpec (someType: Type) =
        match someType with
        | IsPrimitiveType t -> t
        | IsGenericEnumerable t -> t
        | IsRecord t -> t
        | x -> failwithf "Unsupported type '%s'." x.Name
    and (|IsPrimitiveType|_|) (t: Type) = 
        match t.FullName with
        | "System.Int32" -> Some Integer
        | "System.Double" -> Some Float
        | "System.String" -> Some String
        | "System.Boolean" -> Some Bool
        | "Microsoft.FSharp.Core.Unit" -> Some Unit
        | _ -> None
    and (|IsRecord|_|) (t: Type) =
        let isRecord (t: Type) = 
            let attr = t.GetCustomAttribute(typeof<Microsoft.FSharp.Core.CompilationMappingAttribute>)
            if attr <> null then 
                let attr' = attr :?> Microsoft.FSharp.Core.CompilationMappingAttribute
                attr'.SourceConstructFlags &&& SourceConstructFlags.RecordType = SourceConstructFlags.RecordType
            else false
        if isRecord t then
            let properties = t.GetProperties(publicInstance ||| BindingFlags.GetProperty)
            let fields = [ for prop in properties -> { name = prop.Name; valueType = getValueSpec prop.PropertyType }]
            Some (Complex fields)
        else None
    and (|IsGenericEnumerable|_|) (someType: Type) =
        let enumerable = someType.GetInterface("IEnumerable`1")
        if enumerable <> null then
            let genericArgument = enumerable.GenericTypeArguments.[0]
            Some (List (getValueSpec genericArgument))
        else None

    let getParamsSpec (parameters: ParameterInfo seq) =
        [ for p in parameters ->
            { name = p.Name; valueType = getValueSpec p.ParameterType} ]

    let getMethodSpec (method: MethodInfo) =
        let name = method.Name
        let parameters = Complex(getParamsSpec(method.GetParameters()))
        let returns = getValueSpec method.ReturnType
        { name = name; parameters = parameters; returns = returns }

    let getMethods (apiType: Type) =
        apiType.GetMethods(publicInstance)
        |> Seq.map getMethodSpec
        |> List.ofSeq

    let serializeApiSpec<'a>() =
        let apiType = typeof<'a>
        let apiName = getApiName apiType
        let apiMethods = getMethods apiType
        { name = apiName; methods = apiMethods }

    open System.IO
    open FSharp.Json

    let saveToFile spec path =
        if Path.IsPathRooted(path) then
            if File.Exists path then
                File.Delete path
            File.WriteAllText(path, Json.serialize spec)
        else failwithf "Path '%s' not rooted." path
