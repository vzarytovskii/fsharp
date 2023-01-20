// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

module FSharp.Compiler.BuildGraph

open System
open System.Threading
open System.Threading.Tasks
open System.Globalization
open FSharp.Compiler.DiagnosticsLogger
open System.Runtime.CompilerServices
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Core.CompilerServices.StateMachineHelpers
open Internal.Utilities.Library
open Internal.Utilities.Library.CancellableAutoOpens

[<NoEquality; NoComparison>]
type NodeCode<'T> = unit -> Task<'T>

[<Struct; NoComparison; NoEquality>]
type NodeCodeStateMachineData<'T> =
    [<DefaultValue(false)>]
    val mutable Result: 'T
    [<DefaultValue(false)>]
    val mutable MethodBuilder: AsyncTaskMethodBuilder<'T>
and NodeCodeStateMachine<'TOverall> = ResumableStateMachine<NodeCodeStateMachineData<'TOverall>>
and NodeCodeResumptionFunc<'TOverall> = ResumptionFunc<NodeCodeStateMachineData<'TOverall>>
and NodeCodeResumptionDynamicInfo<'TOverall> = ResumptionDynamicInfo<NodeCodeStateMachineData<'TOverall>>
and NodeCodeCode<'TOverall, 'T> = ResumableCode<NodeCodeStateMachineData<'TOverall>, 'T>

[<NoComparison; NoEquality>]
type NodeCodeBuilder() =
 
    member inline _.Delay(generator: unit -> NodeCodeCode<'TOverall, 'T>) : NodeCodeCode<'TOverall, 'T> =
        NodeCodeCode<'TOverall, 'T>(fun sm -> (generator ()).Invoke(&sm))

    [<DefaultValue>]
    member inline _.Zero() : NodeCodeCode<'TOverall, unit> = ResumableCode.Zero()

    member inline _.Return(value: 'T) : NodeCodeCode<'T, 'T> =
        NodeCodeCode<'T, _>(fun sm -> sm.Data.Result <- value; true)

    member inline _.Combine(task1: NodeCodeCode<'TOverall, unit>, task2: NodeCodeCode<'TOverall, 'T>) : NodeCodeCode<'TOverall, 'T> =
        ResumableCode.Combine(task1, task2)

    member inline _.While([<InlineIfLambda>]guard: unit -> bool, body: NodeCodeCode<'TOverall, unit>) : NodeCodeCode<'TOverall, unit> =
        ResumableCode.While(guard, body)

    member inline _.TryWith(body: NodeCodeCode<'TOverall, 'T>, [<InlineIfLambda>] catch: exn -> NodeCodeCode<'TOverall, 'T>) : NodeCodeCode<'TOverall, 'T> =
        ResumableCode.TryWith(body, catch)

    member inline _.TryFinally(body: NodeCodeCode<'TOverall, 'T>, [<InlineIfLambda>] compensation: unit -> unit) : NodeCodeCode<'TOverall, 'T> =
        ResumableCode.TryFinally( body, ResumableCode<_, _>(fun _ -> compensation (); true))

    member inline _.For (sequence: seq<'T>, [<InlineIfLambda>]body: 'T -> NodeCodeCode<'TOverall, unit>) : NodeCodeCode<'TOverall, unit> =
        ResumableCode.For(sequence, body)

    static member inline RunDynamic(code: NodeCodeCode<'T, 'T>) : NodeCode<'T> =
        let mutable sm = NodeCodeStateMachine<'T>()
        let initialResumptionFunc = NodeCodeResumptionFunc<'T>(fun sm -> code.Invoke(&sm))

        let resumptionInfo =
            { new NodeCodeResumptionDynamicInfo<'T>(initialResumptionFunc) with
                member info.MoveNext(sm) =
                    let mutable savedExn = null

                    try
                        sm.ResumptionDynamicInfo.ResumptionData <- null
                        let step = info.ResumptionFunc.Invoke(&sm)

                        if step then
                            sm.Data.MethodBuilder.SetResult(sm.Data.Result)
                        else
                            let mutable awaiter =
                                sm.ResumptionDynamicInfo.ResumptionData
                                :?> ICriticalNotifyCompletion

                            assert not (isNull awaiter)
                            sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter, &sm)

                    with exn ->
                        savedExn <- exn
                    match savedExn with
                    | null -> ()
                    | exn -> sm.Data.MethodBuilder.SetException exn

                member _.SetStateMachine(sm, state) =
                    sm.Data.MethodBuilder.SetStateMachine(state)
            }

        fun () ->
            sm.ResumptionDynamicInfo <- resumptionInfo
            sm.Data.MethodBuilder <- AsyncTaskMethodBuilder<'T>.Create ()
            sm.Data.MethodBuilder.Start(&sm)
            sm.Data.MethodBuilder.Task

    member inline _.Run(code: NodeCodeCode<'T, 'T>) : NodeCode<'T> =
        if __useResumableCode then
            __stateMachine<NodeCodeStateMachineData<'T>, NodeCode<'T>>
                (MoveNextMethodImpl<_>(fun sm ->
                    __resumeAt sm.ResumptionPoint

                    try
                        let __stack_code_fin = code.Invoke(&sm)

                        if __stack_code_fin then
                            sm.Data.MethodBuilder.SetResult(sm.Data.Result)
                    with exn ->
                        sm.Data.MethodBuilder.SetException exn
                ))
                (SetStateMachineMethodImpl<_>(fun sm state ->
                    sm.Data.MethodBuilder.SetStateMachine(state)
                ))
                (AfterCode<_, NodeCode<'T>>(fun sm ->
                    if
                        isNull SynchronizationContext.Current
                        && obj.ReferenceEquals(TaskScheduler.Current, TaskScheduler.Default)
                    then
                        let mutable sm = sm

                        fun () ->
                            sm.Data.MethodBuilder <- AsyncTaskMethodBuilder<'T>.Create ()
                            sm.Data.MethodBuilder.Start(&sm)
                            sm.Data.MethodBuilder.Task
                    else
                        let sm = sm // copy

                        fun () ->
                            Task.Run<'T>(fun () ->
                                let mutable sm = sm
                                sm.Data.MethodBuilder <- AsyncTaskMethodBuilder<'T>.Create ()
                                sm.Data.MethodBuilder.Start(&sm)
                                sm.Data.MethodBuilder.Task
                            )
                ))
        else
            if
                isNull SynchronizationContext.Current
                && obj.ReferenceEquals(TaskScheduler.Current, TaskScheduler.Default)
            then
                NodeCodeBuilder.RunDynamic(code)
            else

                fun () -> Task.Run<'T>(fun () -> NodeCodeBuilder.RunDynamic (code) ())

[<AutoOpen>]
module internal NodeCodeBuilder =
    let internal node = NodeCodeBuilder()

[<AutoOpen>]
module Extensions =

    type Awaiter<'TResult1, 'Awaiter
         when 'Awaiter :> ICriticalNotifyCompletion
         and  'Awaiter: (member IsCompleted: bool)
         and  'Awaiter: (member GetResult: unit -> 'TResult1)> = 'Awaiter

    type internal NodeCodeBuilder with
        
        [<NoEagerConstraintApplication>]
        static member inline BindDynamic<'TResult1, 'TResult2, 'Awaiter, 'TOverall when Awaiter<'TResult1, 'Awaiter>>
            (sm: byref<_>, [<InlineIfLambda>] getAwaiter: unit -> 'Awaiter, [<InlineIfLambda>] continuation: ('TResult1 -> NodeCodeCode<'TOverall, 'TResult2>)) : bool =

            let mutable awaiter = getAwaiter ()

            let cont =
                (NodeCodeResumptionFunc<'TOverall>(fun sm ->
                    let result = awaiter.GetResult()
                    (continuation result).Invoke(&sm)
                ))

            if awaiter.IsCompleted then
                cont.Invoke(&sm)
            else
                sm.ResumptionDynamicInfo.ResumptionData <-
                    (awaiter :> ICriticalNotifyCompletion)

                sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                false

        [<NoEagerConstraintApplication>]
        member inline _.Bind<'TResult1, 'TResult2, 'Awaiter, 'TOverall when Awaiter<'TResult1, 'Awaiter>>
            ([<InlineIfLambda>] getAwaiter: unit -> 'Awaiter,
             [<InlineIfLambda>] continuation: ('TResult1 -> NodeCodeCode<'TOverall, 'TResult2>)) : NodeCodeCode<'TOverall, 'TResult2> =

            NodeCodeCode<'TOverall, _>(fun sm ->
                if __useResumableCode then
                    //-- RESUMABLE CODE START
                    let mutable awaiter = getAwaiter ()

                    let mutable __stack_fin = true

                    if not awaiter.IsCompleted then
                        // This will yield with __stack_yield_fin = false
                        // This will resume with __stack_yield_fin = true
                        let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                        __stack_fin <- __stack_yield_fin

                    if __stack_fin then
                        let result = awaiter.GetResult()
                        (continuation result).Invoke(&sm)
                    else
                        sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter, &sm)
                        false
                else
                    NodeCodeBuilder.BindDynamic<'TResult1, 'TResult2, 'Awaiter, 'TOverall>(
                        &sm,
                        getAwaiter,
                        continuation
                    )
            )

        [<NoEagerConstraintApplication>]
        member inline this.ReturnFrom<'TResult1, 'TResult2, 'Awaiter, 'TOverall when Awaiter<'TResult1, 'Awaiter>>
            ([<InlineIfLambda>] getAwaiter: unit -> 'Awaiter) : NodeCodeCode<_, _> =
            this.Bind((fun () -> getAwaiter ()), (fun v -> this.Return v))

        [<NoEagerConstraintApplication>]
        member inline this.BindReturn<'TResult1, 'TResult2, 'Awaiter, 'TOverall when Awaiter<'TResult1, 'Awaiter>>
            ([<InlineIfLambda>] getAwaiter: unit -> 'Awaiter, [<InlineIfLambda>] f) : NodeCodeCode<'TResult2, 'TResult2> =
            this.Bind((fun () -> getAwaiter ()), (fun v -> this.Return(f v)))

        
        member inline _.Source(nodeCode: NodeCode<_>) = (nodeCode ()).GetAwaiter
        member inline _.Source(s: #seq<_>) : #seq<_> = s
        member inline _.Source(task: Task) = task.GetAwaiter
        member inline _.Source(task: Task<_>) = task.GetAwaiter
        member inline this.Source(async: Async<_>) = this.Source(async |> Async.StartAsTask)
        member inline this.Source(cancellable: Cancellable<_>) = this.Source(Cancellable.toAsync cancellable)

        member inline _.Using<'Resource, 'TOverall, 'T when 'Resource :> IDisposable>
            (resource: 'Resource, [<InlineIfLambda>] body: 'Resource -> NodeCodeCode<'TOverall, 'T>) =
            ResumableCode.Using(resource, body)

        (*[<NoEagerConstraintApplication>]
        member inline this.MergeSources<'TResult1, 'TResult2, 'Awaiter1, 'Awaiter2
            when Awaiter<'TResult1, 'Awaiter1>
            and  Awaiter<'TResult2, 'Awaiter2>>
            (
                [<InlineIfLambda>] left: unit -> ^Awaiter1,
                [<InlineIfLambda>] right: unit -> ^Awaiter2
            ) : unit -> TaskAwaiter<'TResult1 * 'TResult2> =

            let! builder = 
                node {
                    let leftStarted = left ()
                    let rightStarted = right ()
                    let! leftResult = leftStarted
                    let! rightResult = rightStarted
                    return leftResult, rightResult
                }
            return builder.GetAwaiter()*)

type Microsoft.FSharp.Control.TaskBuilderBase with
    member inline this.ReturnFrom(nodeCode: NodeCode<'T>) = this.ReturnFrom(nodeCode ())

let wrapThreadStaticInfo computation =
    async {
        let diagnosticsLogger = DiagnosticsThreadStatics.DiagnosticsLogger
        let phase = DiagnosticsThreadStatics.BuildPhase

        try
            return! (computation() |> Async.AwaitTask)
        finally
            DiagnosticsThreadStatics.DiagnosticsLogger <- diagnosticsLogger
            DiagnosticsThreadStatics.BuildPhase <- phase
    }

type Async<'T> with
    static member AwaitNodeCode(computation: NodeCode<'T>) =
        wrapThreadStaticInfo computation

[<RequireQualifiedAccess>]
type Node private () =

    //static let cancellationToken = wrapThreadStaticInfo (node { return! Async.CancellationToken })
    //static member CancellationToken = cancellationToken

    static member GetAwaiter (nodeCode: NodeCode<_>) = (nodeCode ()).GetAwaiter

    static member Sequential(computations: NodeCode<'T> seq) =
        node {
            let results = ResizeArray()

            for computation in computations do
                let! res = computation
                results.Add(res)

            return results.ToArray()
        }

    // TODO: Review. Do we really want to just `WhenAll`?
    static member Parallel (computations: NodeCode<'T> seq) =
        node {
            let results = ResizeArray()

            for computation in computations do
                results.Add(computation())
            
            let! results = Task.WhenAll(results)
            return results
        }

    static member RunImmediate(computation: NodeCode<'T>, ct: CancellationToken) =
        let diagnosticsLogger = DiagnosticsThreadStatics.DiagnosticsLogger
        let phase = DiagnosticsThreadStatics.BuildPhase
        try
            try
                let work =
                    node {
                        DiagnosticsThreadStatics.DiagnosticsLogger <- diagnosticsLogger
                        DiagnosticsThreadStatics.BuildPhase <- phase
                        return! computation
                    }
                let task = work()
                task.Wait(ct)
                task.Result
            finally
                DiagnosticsThreadStatics.DiagnosticsLogger <- diagnosticsLogger
                DiagnosticsThreadStatics.BuildPhase <- phase
        with :? AggregateException as ex when ex.InnerExceptions.Count = 1 ->
            raise (ex.InnerExceptions[0])

    static member RunImmediateWithoutCancellation(computation: NodeCode<'T>) =
        Node.RunImmediate(computation, CancellationToken.None)

    static member StartAsTask_ForTesting(computation: NodeCode<'T>, ?ct: CancellationToken) =
        let diagnosticsLogger = DiagnosticsThreadStatics.DiagnosticsLogger
        let phase = DiagnosticsThreadStatics.BuildPhase

        try
            let work =
                async {
                    DiagnosticsThreadStatics.DiagnosticsLogger <- diagnosticsLogger
                    DiagnosticsThreadStatics.BuildPhase <- phase
                    return! computation |> Async.AwaitNodeCode
                }

            Async.StartAsTask(work, cancellationToken = defaultArg ct CancellationToken.None)
        finally
            DiagnosticsThreadStatics.DiagnosticsLogger <- diagnosticsLogger
            DiagnosticsThreadStatics.BuildPhase <- phase

    static member AwaitWaitHandle_ForTesting(waitHandle: WaitHandle) =
        node {
            let diagnosticsLogger = DiagnosticsThreadStatics.DiagnosticsLogger
            let phase = DiagnosticsThreadStatics.BuildPhase

            try
                return! Async.AwaitWaitHandle(waitHandle)
            finally
                DiagnosticsThreadStatics.DiagnosticsLogger <- diagnosticsLogger
                DiagnosticsThreadStatics.BuildPhase <- phase
        }

type private AgentMessage<'T> = GetValue of AsyncReplyChannel<Result<'T, Exception>> * callerCancellationToken: CancellationToken

type private Agent<'T> = MailboxProcessor<AgentMessage<'T>> * CancellationTokenSource

[<RequireQualifiedAccess>]
type private GraphNodeAction<'T> =
    | GetValueByAgent
    | GetValue
    | CachedValue of 'T

[<RequireQualifiedAccess>]
module GraphNode =

    // We need to store the culture for the VS thread that is executing now,
    // so that when the agent in the async lazy object picks up thread from the thread pool we can set the culture
    let mutable culture = CultureInfo(CultureInfo.CurrentUICulture.Name)

    let SetPreferredUILang (preferredUiLang: string option) =
        match preferredUiLang with
        | Some s ->
            culture <- CultureInfo s
#if FX_RESHAPED_GLOBALIZATION
            CultureInfo.CurrentUICulture <- culture
#else
            Thread.CurrentThread.CurrentUICulture <- culture
#endif
        | None -> ()

[<Sealed>]
type GraphNode<'T>(retryCompute: bool, computation: NodeCode<'T>) =

    let gate = obj ()
    let mutable computation = computation
    let mutable requestCount = 0

    let mutable cachedResult: Task<'T> = Unchecked.defaultof<_>
    let mutable cachedResultNode: NodeCode<'T> = Unchecked.defaultof<_>

    let isCachedResultNodeNotNull () =
        not (obj.ReferenceEquals(cachedResultNode, null))

    let isCachedResultNotNull () =
        not (obj.ReferenceEquals(cachedResult, null))

    // retryCompute indicates that we abandon computations when the originator is
    // cancelled.
    //
    // If retryCompute is 'true', the computation is run directly in the originating requestor's
    // thread.  If cancelled, other awaiting computations must restart the computation from scratch.
    //
    // If retryCompute is 'false', a MailboxProcessor is used to allow the cancelled originator
    // to detach from the computation, while other awaiting computations continue to wait on the result.
    //
    // Currently, 'retryCompute' = true for all graph nodes. However, the code for we include the
    // code to allow 'retryCompute' = false in case it's needed in the future, and ensure it is under independent
    // unit test.
    let loop (agent: MailboxProcessor<AgentMessage<'T>>) =
        async {
            assert (not retryCompute)

            try
                while true do
                    match! agent.Receive() with
                    | GetValue (replyChannel, callerCancellationToken) ->

                        Thread.CurrentThread.CurrentUICulture <- GraphNode.culture

                        try
                            use _reg =
                                // When a cancellation has occured, notify the reply channel to let the requester stop waiting for a response.
                                callerCancellationToken.Register(fun () ->
                                    let ex = OperationCanceledException() :> exn
                                    replyChannel.Reply(Result.Error ex))

                            callerCancellationToken.ThrowIfCancellationRequested()

                            if isCachedResultNotNull () then
                                replyChannel.Reply(Ok cachedResult.Result)
                            else
                                // This computation can only be canceled if the requestCount reaches zero.
                                let! result = computation |> Async.AwaitNodeCode
                                cachedResult <- Task.FromResult(result)
                                cachedResultNode <- node { return result }
                                computation <- Unchecked.defaultof<_>

                                if not callerCancellationToken.IsCancellationRequested then
                                    replyChannel.Reply(Ok result)
                        with ex ->
                            if not callerCancellationToken.IsCancellationRequested then
                                replyChannel.Reply(Result.Error ex)
            with _ ->
                ()
        }

    let mutable agent: Agent<'T> = Unchecked.defaultof<_>

    let semaphore: SemaphoreSlim =
        if retryCompute then
            new SemaphoreSlim(1, 1)
        else
            Unchecked.defaultof<_>

    member _.GetOrComputeValue() =
        // fast path
        if isCachedResultNodeNotNull () then
            cachedResultNode
        else
            node {
                if isCachedResultNodeNotNull () then
                    return! cachedResult
                else
                    let action =
                        lock gate
                        <| fun () ->
                            // We try to get the cached result after the lock so we don't spin up a new mailbox processor.
                            if isCachedResultNodeNotNull () then
                                GraphNodeAction<'T>.CachedValue cachedResult.Result
                            else
                                requestCount <- requestCount + 1

                                if retryCompute then
                                    GraphNodeAction<'T>.GetValue
                                else
                                    match box agent with
                                    | null ->
                                        try
                                            let cts = new CancellationTokenSource()
                                            let mbp = new MailboxProcessor<_>(loop, cancellationToken = cts.Token)
                                            let newAgent = (mbp, cts)
                                            agent <- newAgent
                                            mbp.Start()
                                            GraphNodeAction<'T>.GetValueByAgent
                                        with exn ->
                                            agent <- Unchecked.defaultof<_>
                                            PreserveStackTrace exn
                                            raise exn
                                    | _ -> GraphNodeAction<'T>.GetValueByAgent

                    match action with
                    | GraphNodeAction.CachedValue result -> return result
                    | GraphNodeAction.GetValue ->
                        try
                            // TODO: Probably shouldn't reuse async's ct
                            let! ct = Async.CancellationToken 

                            // We must set 'taken' before any implicit cancellation checks
                            // occur, making sure we are under the protection of the 'try'.
                            // For example, NodeCode's 'try/finally' (TryFinally) uses async.TryFinally which does
                            // implicit cancellation checks even before the try is entered, as do the
                            // de-sugaring of 'do!' and other NodeCode constructs.
                            let mutable taken = false

                            try
                                do!
                                    semaphore
                                        .WaitAsync(ct)
                                        .ContinueWith(
                                            (fun _ -> taken <- true),
                                            (TaskContinuationOptions.NotOnCanceled
                                             ||| TaskContinuationOptions.NotOnFaulted
                                             ||| TaskContinuationOptions.ExecuteSynchronously)
                                        )

                                if isCachedResultNotNull () then
                                    return cachedResult.Result
                                else

                                    let! res = computation
                                    cachedResult <- Task.FromResult(res)
                                    cachedResultNode <- node { return res }
                                    computation <- Unchecked.defaultof<_>
                                    return res
                            finally
                                if taken then semaphore.Release() |> ignore
                        finally
                            lock gate <| fun () -> requestCount <- requestCount - 1

                    | GraphNodeAction.GetValueByAgent ->
                        assert (not retryCompute)
                        let mbp, cts = agent

                        try
                            let! ct = Async.CancellationToken
                            let! res = mbp.PostAndAsyncReply(fun replyChannel -> GetValue(replyChannel, ct))
                                

                            match res with
                            | Ok result -> return result
                            | Result.Error ex -> return raise ex
                        finally
                            lock gate
                            <| fun () ->
                                requestCount <- requestCount - 1

                                if requestCount = 0 then
                                    cts.Cancel() // cancel computation when all requests are cancelled

                                    try
                                        (mbp :> IDisposable).Dispose()
                                    with _ ->
                                        ()

                                    cts.Dispose()
                                    agent <- Unchecked.defaultof<_>
            }

    member _.TryPeekValue() =
        match box cachedResult with
        | null -> ValueNone
        | _ -> ValueSome cachedResult.Result

    member _.HasValue = isCachedResultNotNull ()

    member _.IsComputing = requestCount > 0

    new(computation) = GraphNode(retryCompute = true, computation = computation)
