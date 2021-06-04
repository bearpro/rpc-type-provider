module RpcTypeExporter.ApiSpecification

open System
open System.Reflection
open System.Collections.Generic

type ValueSpec =
  | Unit
  | Integer
  | Float
  | String
  | Bool
  | List of valueType: ValueSpec
  | Complex of typeName: string * fields: Named list
and Named = { name: string; valueType: ValueSpec }

type MethodSpec = 
  { name: string 
    returns: ValueSpec
    parameters: ValueSpec }

type ApiSpec = 
  { name: string
    methods: MethodSpec list }

type SerializationContext =
    { complexTypeMap: Map<ValueSpec, Type> }
    with
        static member Empty = { complexTypeMap = Map.empty }

let publicInstance = BindingFlags.Public ||| BindingFlags.Instance

let getApiName (apiType: Type) =
    let typeName = apiType.Name
    if typeName.[0] = 'I' then
        typeName.[1..]
    else
        typeName

let rec getSpec ctx (someType: Type): ValueSpec * SerializationContext =
    match (someType, ctx) with
    | IsPrimitiveType (t, ctx) -> t, ctx
    | IsGenericEnumerable (t, ctx) -> t, ctx
    | IsRecord (t, ctx) -> t, ctx
    | (x, _) -> failwithf "Unsupported type '%s'." x.Name
and (|IsPrimitiveType|_|) (t: Type, ctx) = 
    match t.FullName with
    | "System.Int32" -> Some (Integer, ctx)
    | "System.Double" -> Some (Float, ctx)
    | "System.String" -> Some (String, ctx)
    | "System.Boolean" -> Some (Bool, ctx)
    | "Microsoft.FSharp.Core.Unit" -> Some (Unit, ctx)
    | _ -> None
and (|IsRecord|_|) (t: Type, ctx) =
    let isRecord (t: Type) = 
        let attr = t.GetCustomAttribute(typeof<Microsoft.FSharp.Core.CompilationMappingAttribute>)
        if attr <> null then 
            let attr' = attr :?> Microsoft.FSharp.Core.CompilationMappingAttribute
            attr'.SourceConstructFlags &&& SourceConstructFlags.RecordType = SourceConstructFlags.RecordType
        else false
    if isRecord t then
        let properties = t.GetProperties(publicInstance ||| BindingFlags.GetProperty)
        let folder (specs, ctx) (propi: PropertyInfo) =
            let propspec, ctx = getSpec ctx propi.PropertyType
            ({ name = propi.Name; valueType = propspec } :: specs, ctx)
        let fields, ctx = properties |> Seq.fold folder ([], ctx)
        let spec = Complex(t.Name, fields |> List.rev)
        let ctx' = { ctx with complexTypeMap = ctx.complexTypeMap.Add(spec, t)}
        Some(spec, ctx')
    else None
and (|IsGenericEnumerable|_|) (someType: Type, ctx) =
    let enumerable = someType.GetInterface("IEnumerable`1")
    if enumerable <> null then
        let genericArgument = enumerable.GenericTypeArguments.[0]
        let spec, ctx' = getSpec ctx genericArgument
        Some (List spec, ctx')
    else None

let getParamsSpec ctx (parameters: ParameterInfo seq) =
    let folder (specs, ctx) (parami: ParameterInfo) =
        let spec, ctx' = getSpec ctx parami.ParameterType
        { name = parami.Name; valueType = spec } :: specs, ctx'

    let parmsSpecs, ctx = Seq.fold folder ([], ctx) parameters
    
    (Seq.rev >> List.ofSeq) parmsSpecs, ctx

let getMethodSpec ctx (method: MethodInfo) =
    let name = method.Name
    let parmItemsSpec, ctx' = getParamsSpec ctx (method.GetParameters())
    let parameters = Complex($"{name}.params", parmItemsSpec)
    let returns, ctx'' = getSpec ctx' method.ReturnType
    { name = name; parameters = parameters; returns = returns }, ctx''

let getMethods ctx (apiType: Type) =
    let folder (methods: MethodSpec list, ctx: SerializationContext) mi =
        let spec, ctx' = getMethodSpec ctx mi
        spec :: methods, ctx'

    let methods, ctx' = apiType.GetMethods(publicInstance) |> Array.fold folder ([], ctx)
    List.rev methods, ctx'


let serializeApiSpec<'a>() =
    let ctx = { complexTypeMap = Map.empty }
    let apiType = typeof<'a>
    let apiName = getApiName apiType
    let apiMethods, ctx = getMethods ctx apiType
    { name = apiName; methods = apiMethods }, ctx

open System.IO
open FSharp.Json

let saveToFile spec path =
    if Path.IsPathRooted(path) then
        if File.Exists path then
            File.Delete path
        File.WriteAllText(path, Json.serialize spec)
    else failwithf "Path '%s' not rooted." path
