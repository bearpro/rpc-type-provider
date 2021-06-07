module RpcTypeProviderTests


open MyNamespace
open NUnit.Framework

type Generative2 = Sample.GenerativeProvider<2>
type Generative4 = Sample.GenerativeProvider<4>

[<Test>]
let ``Can access properties of generative provider 2`` () =
    let obj = Generative2()
    Assert.AreEqual(obj.Property1, 1)
    Assert.AreEqual(obj.Property2, 2)

[<Test>]
let ``Can access properties of generative provider 4`` () =
    let obj = Generative4()
    Assert.AreEqual(obj.Property1, 1)
    Assert.AreEqual(obj.Property2, 2)
    Assert.AreEqual(obj.Property3, 3)
    Assert.AreEqual(obj.Property4, 4)

[<Literal>]
let SimpleSpecLocation = @"C:\Users\mprokazin\source\repos\rpc-type-provider\sample\SimpleSampleB.spec.json"
type SimpleRpc = RpcTypeProvider.RpcTypeProvider<SimpleSpecLocation>

let sum = SimpleRpc.Sum(1, 2)

[<Literal>]
let Source = @"C:\Users\mprokazin\source\repos\rpc-type-provider\sample\ComplexSampleB.spec.json"
type X = RpcTypeProvider.RpcTypeProvider<Source>

[<Test>]
let ``Fields assigned`` () =
    let book = X.Book("", 2002, "", "", "")
    Assert.AreEqual(book.PublicationYear, 2002)
    
