namespace MyNamespace

open System

// Put any utilities here
[<AutoOpen>]
module internal Utilities = 
    let x = 1

// Put any runtime constructs here
type DataSource(filename:string) = 
    member this.FileName = filename

type HttpMethod = Get | Post | Put | Delete

type ParamType =
    | Unit
    | Integer
    | Float
    | String
    | List of ParamType

type InterfaceMethod =
  { Endpoint: string
    MethodName: string
    HttpMethod: HttpMethod
    Remark: string
    Params: ParamType list
    Returns: ParamType }


// Put the TypeProviderAssemblyAttribute in the runtime DLL, pointing to the design-time DLL
[<assembly:CompilerServices.TypeProviderAssembly("RpcTypeProvider.DesignTime.dll")>]
do ()
