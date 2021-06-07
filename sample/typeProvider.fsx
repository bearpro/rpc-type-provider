#r "..\\RpcTypeProvider\\src\\RpcTypeProvider.Runtime\\bin\\Debug\\typeproviders\\fsharp41\\netcoreapp3.1\\RpcTypeProvider.dll"

[<Literal>] let url = "http://localhost:5000/SampleApi"
type Rpc = RpcTypeProvider.RpcTypeProvider<url>
