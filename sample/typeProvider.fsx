#r "..\\RpcTypeProvider\\src\\RpcTypeProvider.Runtime\\bin\\Debug\\typeproviders\\fsharp41\\netstandard2.0\\RpcTypeProvider.DesignTime.dll"

[<Literal>] let url = "http://localhost:5000/SampleApi"
type Rpc = RpcTypeProvider.RpcTypeProvider<url>
