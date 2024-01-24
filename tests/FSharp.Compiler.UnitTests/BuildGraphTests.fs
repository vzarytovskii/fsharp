// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.
namespace FSharp.Compiler.UnitTests

open System
open System.Threading
open System.Runtime.CompilerServices
open Xunit
open FSharp.Test
open FSharp.Test.Compiler
open FSharp.Compiler.BuildGraph
open Internal.Utilities.Library

module BuildGraphTests =
    open FSharp.Compiler.Facilities.CancellableTasks
    
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let private createNode () =
        let o = obj ()
        GraphNode(
            cancellableTask { 
                Assert.shouldBeTrue (o <> null)
                return 1 
            }
        ), WeakReference(o)

    [<Fact>]
    let ``Intialization of graph node should not have a computed value``() =
        let node = GraphNode(CancellableTask.singleton(1))
        Assert.shouldBeTrue(node.TryPeekValue().IsNone)
        Assert.shouldBeFalse(node.HasValue)

    [<Fact>]
    let ``Two requests to get a value asynchronously should be successful``() =
        let resetEvent = new ManualResetEvent(false)
        let resetEventInAsync = new ManualResetEvent(false)

        let graphNode = 
            GraphNode(
                cancellableTask { 
                    resetEventInAsync.Set() |> ignore
                    let! _ = Async.AwaitWaitHandle(resetEvent)
                    return 1 
                }
            )

        let task1 =
            cancellableTask {
                let! _ = graphNode.GetOrComputeValue()
                ()
            } |> CancellableTask.startWithoutCancellation

        let task2 =
            cancellableTask {
                let! _ = graphNode.GetOrComputeValue()
                ()
            } |> CancellableTask.startWithoutCancellation

        resetEventInAsync.WaitOne() |> ignore
        resetEvent.Set() |> ignore
        try
            task1.Wait(1000) |> ignore
            task2.Wait() |> ignore
        with
        | :? TimeoutException -> reraise()
        | _ -> ()

    [<Fact>]
    let ``Many requests to get a value asynchronously should only evaluate the computation once``() =
        let requests = 10000
        let mutable computationCount = 0

        let graphNode =
            GraphNode(
                cancellableTask {
                    computationCount <- computationCount + 1
                    return 1
                }
            )

        let work = CancellableTask.whenAll(Array.init requests (fun _ -> graphNode.GetOrComputeValue()))

        CancellableTask.runSynchronouslyWithoutCancellation work
        |> ignore

        Assert.shouldBe 1 computationCount

    [<Fact>]
    let ``Many requests to get a value asynchronously should get the correct value``() =
        let requests = 10000

        let graphNode = GraphNode(CancellableTask.singleton(1))

        let work = CancellableTask.whenAll(Array.init requests (fun _ -> graphNode.GetOrComputeValue()))
        let result = CancellableTask.runSynchronouslyWithoutCancellation work

        Assert.shouldNotBeEmpty result
        Assert.shouldBe requests result.Length
        result
        |> Seq.iter (Assert.shouldBe 1)

    [<Fact>]
    let ``A request to get a value asynchronously should have its computation cleaned up by the GC``() =
        let graphNode, weak = createNode ()

        GC.Collect(2, GCCollectionMode.Forced, true)

        Assert.shouldBeTrue weak.IsAlive

        CancellableTask.runSynchronouslyWithoutCancellation (graphNode.GetOrComputeValue())

        |> ignore

        GC.Collect(2, GCCollectionMode.Forced, true)

        Assert.shouldBeFalse weak.IsAlive

    [<Fact>]
    let ``Many requests to get a value asynchronously should have its computation cleaned up by the GC``() =
        let requests = 10000

        let graphNode, weak = createNode ()

        GC.Collect(2, GCCollectionMode.Forced, true)
        
        Assert.shouldBeTrue weak.IsAlive

        CancellableTask.runSynchronouslyWithoutCancellation(CancellableTask.whenAll(Array.init requests (fun _ -> graphNode.GetOrComputeValue()))) |> ignore

        GC.Collect(2, GCCollectionMode.Forced, true)

        Assert.shouldBeFalse weak.IsAlive

    [<Fact>]
    let ``A request can cancel``() =
        let graphNode = 
            GraphNode(
                cancellableTask { 
                    return 1 
                }
            )

        use cts = new CancellationTokenSource()

        let work =
            cancellableTask {
                cts.Cancel()
                return! graphNode.GetOrComputeValue()
            }

        let ex =
            try
                CancellableTask.runSynchronously cts.Token work
                |> ignore
                failwith "Should have canceled"
            with
            | :? OperationCanceledException as ex ->
                ex

        Assert.shouldBeTrue(ex <> null)

    [<Fact>]
    let ``A request can cancel 2``() =
        let resetEvent = new ManualResetEvent(false)

        let graphNode = 
            GraphNode(
                cancellableTask { 
                    let! _ = Async.AwaitWaitHandle(resetEvent)
                    return 1 
                }
            )

        use cts = new CancellationTokenSource()

        let task =
            cancellableTask {
                cts.Cancel()
                resetEvent.Set() |> ignore
            } |> CancellableTask.startWithoutCancellation

        let ex =
            try
                CancellableTask.runSynchronously cts.Token (graphNode.GetOrComputeValue())
                |> ignore
                failwith "Should have canceled"
            with
            | :? OperationCanceledException as ex ->
                ex

        Assert.shouldBeTrue(ex <> null)
        try task.Wait(1000) |> ignore with | :? TimeoutException -> reraise() | _ -> ()

    [<Fact>]
    let ``Many requests to get a value asynchronously might evaluate the computation more than once even when some requests get canceled``() =
        let requests = 10000
        let resetEvent = new ManualResetEvent(false)
        let mutable computationCountBeforeSleep = 0
        let mutable computationCount = 0

        let graphNode = 
            GraphNode(
                cancellableTask { 
                    computationCountBeforeSleep <- computationCountBeforeSleep + 1
                    let! _ = Async.AwaitWaitHandle(resetEvent)
                    computationCount <- computationCount + 1
                    return 1 
                }
            )

        use cts = new CancellationTokenSource()

        let work = 
            cancellableTask { 
                let! _ = graphNode.GetOrComputeValue()
                ()
            }

        let tasks = ResizeArray()

        for i = 0 to requests - 1 do
            if i % 10 = 0 then
                CancellableTask.start cts.Token work
                |> tasks.Add
            else
                CancellableTask.startWithoutCancellation work
                |> tasks.Add

        cts.Cancel()
        resetEvent.Set() |> ignore

        CancellableTask.runSynchronouslyWithoutCancellation work
        |> ignore

        Assert.shouldBeTrue cts.IsCancellationRequested
        Assert.shouldBeTrue(computationCountBeforeSleep > 0)
        Assert.shouldBeTrue(computationCount >= 0)

        tasks
        |> Seq.iter (fun x -> 
            try x.Wait(1000) |> ignore with | :? TimeoutException -> reraise() | _ -> ())

    [<Fact>]
    let ``GraphNode created from an already computed result will return it in tryPeekValue`` () =
        let graphNode = GraphNode.FromResult 1

        Assert.shouldBeTrue graphNode.HasValue
        Assert.shouldBe (ValueSome 1) (graphNode.TryPeekValue())
