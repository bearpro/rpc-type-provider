namespace RpcTypeExporter

open System
open System.Reflection

type Value =
  | Unit
  | Integer
  | Float
  | String
  | List of valueType: Value
  | Complex of Named list
and Named = { name: string; valueType: Value }

type Method = 
  { name: string 
    returns: Value
    parameters: Value }

type Api = 
  { name: string
    methods: Method list }

module SpecificationSerializer =

    let publicInstance = BindingFlags.Public ||| BindingFlags.Instance

    let getApiName (apiType: Type) =
        let typeName = apiType.Name
        if typeName.[0] = 'I' then
            typeName.[1..]
        else
            typeName
    
    let getTypeValue (someType: Type) =
        match someType.FullName with
        | "System.Int32" -> Integer
        | t -> failwithf "Unsupported type '%s'." t

    let getParamsSpec (parameters: ParameterInfo seq) =
        [ for p in parameters ->
            { name = p.Name; valueType = getTypeValue p.ParameterType} ]

    let getMethodSpec (method: MethodInfo) =
        let name = method.Name
        let parameters = Complex(getParamsSpec(method.GetParameters()))
        let returns = getTypeValue method.ReturnType
        { name = name; parameters = parameters; returns = returns }

    let getMethods (apiType: Type) =
        apiType.GetMethods(publicInstance)
        |> Seq.map getMethodSpec
        |> List.ofSeq

    let serilizeApiSpec<'a>() =
        let apiType = typeof<'a>
        let apiName = getApiName apiType
        let apiMethods = getMethods apiType
        { name = apiName; methods = apiMethods }
