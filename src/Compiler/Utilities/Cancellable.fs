namespace Internal.Utilities.Library

#nowarn "3513"

open System
open System.Threading
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Core.CompilerServices.StateMachineHelpers

[<RequireQualifiedAccess; Struct; NoComparison>]
type ValueOrCancelled<'TResult> =
    | Value of result: 'TResult
    | Cancelled of ``exception``: OperationCanceledException

[<NoEquality; NoComparison>]
type Cancellable<'T> = CancellationToken -> ValueOrCancelled<'T>

[<Struct; NoComparison; NoEquality>]
type CancellableStateMachineData<'T> =
    [<DefaultValue(false)>]
    val mutable CancellationToken: CancellationToken
    [<DefaultValue(false)>]
    val mutable Result: ValueOrCancelled<'T>
    member inline this.IsCancellationRequested() = this.CancellationToken.IsCancellationRequested

type CancellableStateMachine<'TOverall> = ResumableStateMachine<CancellableStateMachineData<'TOverall>>
type CancellableResumptionFunc<'TOverall> = ResumptionFunc<CancellableStateMachineData<'TOverall>>
type CancellableResumptionDynamicInfo<'TOverall> = ResumptionDynamicInfo<CancellableStateMachineData<'TOverall>>
type CancellableCode<'TOverall, 'T> = ResumableCode<CancellableStateMachineData<'TOverall>, 'T>

[<Sealed; NoComparison; NoEquality>]
type CancellableBuilder() =

    [<DefaultValue>]
    member inline _.Zero() : CancellableCode<'TOverall, unit> = ResumableCode.Zero()

    member inline _.Combine(cancellable1: CancellableCode<'TOverall, unit>, cancellable2: CancellableCode<'TOverall, 'T>) : CancellableCode<'TOverall, 'T> =
        ResumableCode.Combine(cancellable1, cancellable2)

    member inline _.Delay([<InlineIfLambda>] f: unit -> CancellableCode<'TOverall, 'T>) : CancellableCode<'TOverall, 'T> =
        ResumableCode.Delay(fun () ->
            CancellableCode(fun sm ->
                if sm.Data.IsCancellationRequested() then
                    sm.Data.Result <- ValueOrCancelled.Cancelled(OperationCanceledException sm.Data.CancellationToken)
                    true
                else
                    (f ()).Invoke(&sm)))

    member inline _.Return(value: 'T) : CancellableCode<'T, 'T> =
        CancellableCode<'T, _>(fun sm ->
            if sm.Data.IsCancellationRequested() then
                sm.Data.Result <- ValueOrCancelled.Cancelled(OperationCanceledException sm.Data.CancellationToken)
            else
                sm.Data.Result <- ValueOrCancelled.Value value
            true
        )

    member inline _.Bind<'T>(f: Cancellable<'T>, continuation: 'T -> CancellableCode<'T, 'T>) : CancellableCode<'T, 'T> =
        CancellableCode<'T, _>(fun sm ->
            if sm.Data.IsCancellationRequested() then
                sm.Data.Result <- ValueOrCancelled.Cancelled(OperationCanceledException sm.Data.CancellationToken)
                true
            else
                let result = f sm.Data.CancellationToken
                match result with
                | ValueOrCancelled.Cancelled(ex) ->
                    sm.Data.Result <- ValueOrCancelled.Cancelled(ex)
                    true
                | ValueOrCancelled.Value(v) -> (continuation v).Invoke(&sm)
        )

    member inline _.TryWith(computation: CancellableCode<'TOverall, 'T>, [<InlineIfLambda>] catchHandler: Exception -> CancellableCode<'TOverall, 'T>) : CancellableCode<'TOverall, 'T> =
        ResumableCode.TryWith(
            CancellableCode(fun sm ->
                if sm.Data.IsCancellationRequested() then
                    sm.Data.Result <- ValueOrCancelled.Cancelled(OperationCanceledException sm.Data.CancellationToken)
                    true
                else
                    (computation).Invoke(&sm)
            ),
            catchHandler
        )

    member inline _.Run(code: CancellableCode<'T, 'T>) : Cancellable<'T> =
        if __useResumableCode then
            __stateMachine<CancellableStateMachineData<'T>, Cancellable<'T>>
                (MoveNextMethodImpl<_>(fun sm ->
                    __resumeAt sm.ResumptionPoint
                    let __stack_code_fin = code.Invoke(&sm)
                    if __stack_code_fin then // We're done
                        sm.ResumptionPoint <- -1))
                (SetStateMachineMethodImpl<_>(fun _ _ ->()))
                (AfterCode<_, _>(fun sm ->
                    let mutable sm = sm
                    fun (ct) ->
                        if ct.IsCancellationRequested then
                            ValueOrCancelled.Cancelled(OperationCanceledException ct)
                        else
                            sm.Data.CancellationToken <- ct
                            let __stack_code_fin = code.Invoke(&sm)
                            if __stack_code_fin then
                                sm.ResumptionPoint <- -1

                            sm.Data.Result
                        ))
        else
            failwith "TODO: Run"

    (*
    member inline _.Bind<'T, 'TResult, 'TOverall>([<InlineIfLambda>] cancellable: Cancellable<'T>, [<InlineIfLambda>] continuation: ValueOrCancelled<'T> -> CancellableCode<'TOverall, 'TResult>) : CancellableCode<'TOverall, 'TResult> =
        CancellableCode<'TOverall, _>(
            fun sm ->
                if __useResumableCode then
                    if sm.Data.IsCancellationRequested() then
                        sm.Data.Result <- ValueOrCancelled.Cancelled(OperationCanceledException sm.Data.CancellationToken)
                        true
                    else
                        // Since it's fully synchronous, we don't need to try and yield
                        let result = cancellable sm.Data.CancellationToken
                        (continuation result).Invoke(&sm)
                else
                    failwith "TODO"
        )
    *)
    (* TODO: Do we need it, or just bind+return is enough?
    member inline _.BindReturn<'T, 'TResult>([<InlineIfLambda>] cancellable: Cancellable<'T>, [<InlineIfLambda>] continuation: 'T -> 'TResult) : CancellableCode<'TResult, 'TResult> =
        CancellableCode<'TResult, _>(
            fun sm ->
                if __useResumableCode then
                    if sm.Data.IsCancellationRequested() then
                        sm.Data.Result <- ValueOrCancelled.Cancelled(OperationCanceledException sm.Data.CancellationToken)
                        true
                    else
                        // Since it's fully synchronous, we don't need to try and yield
                        let result = cancellable sm.Data.CancellationToken
                        match result with
                        | ValueOrCancelled.Value value ->
                            sm.Data.Result <- ValueOrCancelled.Value(continuation value)
                        | ValueOrCancelled.Cancelled e ->
                            sm.Data.Result <- ValueOrCancelled.Cancelled(e)
                        true
                else
                    failwith "TODO"
        )
        *)

    (*
    

    member inline this.ReturnFrom([<InlineIfLambda>] computation: Cancellable<'T>) =
        CancellableCode<'T, _>(fun sm ->
            if sm.Data.IsCancellationRequested() then
                sm.Data.Result <- ValueOrCancelled.Cancelled(OperationCanceledException sm.Data.CancellationToken)
            else
                sm.Data.Result <- computation sm.Data.CancellationToken
            true
        )

    
    *)


[<AbstractClass; Sealed; NoComparison; NoEquality>]
type Cancellable =
    [<ThreadStatic; DefaultValue>]
    static val mutable private tokens: CancellationToken list

    static let disposable =
        { new IDisposable with
            member _.Dispose() =
                Cancellable.Tokens <- List.tail Cancellable.Tokens }

    static member Tokens
        with private get () =
            match box Cancellable.tokens with
            | Null -> []
            | _ -> Cancellable.tokens
        and private set v = Cancellable.tokens <- v

    static member UsingToken(ct) =
        Cancellable.Tokens <- ct :: Cancellable.Tokens
        disposable

    static member Token =
        match Cancellable.Tokens with
        | [] -> CancellationToken.None
        | token :: _ -> token

module Cancellable =
    let inline run (ct: CancellationToken) ([<InlineIfLambda>] cancellable) =
        if ct.IsCancellationRequested then
            ValueOrCancelled.Cancelled(OperationCanceledException ct)
        else
            try
                use _ = Cancellable.UsingToken(ct)
                cancellable ct
            with :? OperationCanceledException as e ->
                ValueOrCancelled.Cancelled(OperationCanceledException e.CancellationToken)

    let fold f acc seq =
        fun ct ->
            let mutable acc = ValueOrCancelled.Value acc

            for x in seq do
                match acc with
                | ValueOrCancelled.Value accv -> acc <- run ct (f accv x)
                | ValueOrCancelled.Cancelled _ -> ()

            acc

    let inline runWithoutCancellation ([<InlineIfLambda>] cancellable) =
        match run CancellationToken.None cancellable with
        | ValueOrCancelled.Cancelled _ -> failwith "unexpected cancellation"
        | ValueOrCancelled.Value r -> r


(*
[<Sealed; NoComparison; NoEquality>]
type Cancellable =
    [<ThreadStatic; DefaultValue>]
    static val mutable private tokens: CancellationToken list

    static let disposable =
        { new IDisposable with
            member this.Dispose() =
                Cancellable.Tokens <- Cancellable.Tokens |> List.tail
        }

    static member Tokens
        with private get () =
            match box Cancellable.tokens with
            | Null -> []
            | _ -> Cancellable.tokens
        and private set v = Cancellable.tokens <- v

    static member UsingToken(ct) =
        Cancellable.Tokens <- ct :: Cancellable.Tokens
        disposable

    static member Token =
        match Cancellable.Tokens with
        | [] -> CancellationToken.None
        | token :: _ -> token

    /// There may be multiple tokens if `UsingToken` is called multiple times, producing scoped structure.
    /// We're interested in the current, i.e. the most recent, one.
    static member CheckAndThrow() =
        match Cancellable.Tokens with
        | [] -> ()
        | token :: _ -> token.ThrowIfCancellationRequested()

namespace Internal.Utilities.Library

open System
open System.Threading
open FSharp.Compiler

#if !FSHARPCORE_USE_PACKAGE
open FSharp.Core.CompilerServices.StateMachineHelpers
#endif

[<RequireQualifiedAccess; Struct; NoComparison>]
type ValueOrCancelled<'TResult> =
    | Value of result: 'TResult
    | Cancelled of ``exception``: OperationCanceledException

[<Struct; NoComparison; NoEquality>]
type Cancellable<'T> = Cancellable of (CancellationToken -> ValueOrCancelled<'T>)
*)

(*
module Cancellable =

    let inline run (ct: CancellationToken) (Cancellable oper) =
        if ct.IsCancellationRequested then
            ValueOrCancelled.Cancelled(OperationCanceledException ct)
        else
            try
                use _ = Cancellable.UsingToken(ct)
                oper ct
            with :? OperationCanceledException as e ->
                ValueOrCancelled.Cancelled(OperationCanceledException e.CancellationToken)

    let fold f acc seq =
        Cancellable(fun ct ->
            let mutable acc = ValueOrCancelled.Value acc

            for x in seq do
                match acc with
                | ValueOrCancelled.Value accv -> acc <- run ct (f accv x)
                | ValueOrCancelled.Cancelled _ -> ()

            acc)

    let runWithoutCancellation comp =
        let res = run CancellationToken.None comp

        match res with
        | ValueOrCancelled.Cancelled _ -> failwith "unexpected cancellation"
        | ValueOrCancelled.Value r -> r

    let toAsync c =
        async {
            let! ct = Async.CancellationToken
            let res = run ct c

            return!
                Async.FromContinuations(fun (cont, _econt, ccont) ->
                    match res with
                    | ValueOrCancelled.Value v -> cont v
                    | ValueOrCancelled.Cancelled ce -> ccont ce)
        }

    let token () = Cancellable(ValueOrCancelled.Value)
*)

(*
type CancellableBuilder() =

    member inline _.Delay([<InlineIfLambda>] f) =
        Cancellable(fun ct ->
            let (Cancellable g) = f ()
            g ct)

    member inline _.Bind(comp, [<InlineIfLambda>] k) =
        Cancellable(fun ct ->
#if !FSHARPCORE_USE_PACKAGE
            __debugPoint ""
#endif

            match Cancellable.run ct comp with
            | ValueOrCancelled.Value v1 -> Cancellable.run ct (k v1)
            | ValueOrCancelled.Cancelled err1 -> ValueOrCancelled.Cancelled err1)

    member inline _.BindReturn(comp, [<InlineIfLambda>] k) =
        Cancellable(fun ct ->
#if !FSHARPCORE_USE_PACKAGE
            __debugPoint ""
#endif

            match Cancellable.run ct comp with
            | ValueOrCancelled.Value v1 -> ValueOrCancelled.Value(k v1)
            | ValueOrCancelled.Cancelled err1 -> ValueOrCancelled.Cancelled err1)

    member inline _.Combine(comp1, comp2) =
        Cancellable(fun ct ->
#if !FSHARPCORE_USE_PACKAGE
            __debugPoint ""
#endif

            match Cancellable.run ct comp1 with
            | ValueOrCancelled.Value() -> Cancellable.run ct comp2
            | ValueOrCancelled.Cancelled err1 -> ValueOrCancelled.Cancelled err1)

    member inline _.TryWith(comp, [<InlineIfLambda>] handler) =
        Cancellable(fun ct ->
#if !FSHARPCORE_USE_PACKAGE
            __debugPoint ""
#endif

            let compRes =
                try
                    match Cancellable.run ct comp with
                    | ValueOrCancelled.Value res -> ValueOrCancelled.Value(Choice1Of2 res)
                    | ValueOrCancelled.Cancelled exn -> ValueOrCancelled.Cancelled exn
                with err ->
                    ValueOrCancelled.Value(Choice2Of2 err)

            match compRes with
            | ValueOrCancelled.Value res ->
                match res with
                | Choice1Of2 r -> ValueOrCancelled.Value r
                | Choice2Of2 err -> Cancellable.run ct (handler err)
            | ValueOrCancelled.Cancelled err1 -> ValueOrCancelled.Cancelled err1)

    member inline _.Using(resource, [<InlineIfLambda>] comp) =
        Cancellable(fun ct ->
#if !FSHARPCORE_USE_PACKAGE
            __debugPoint ""
#endif
            let body = comp resource

            let compRes =
                try
                    match Cancellable.run ct body with
                    | ValueOrCancelled.Value res -> ValueOrCancelled.Value(Choice1Of2 res)
                    | ValueOrCancelled.Cancelled exn -> ValueOrCancelled.Cancelled exn
                with err ->
                    ValueOrCancelled.Value(Choice2Of2 err)

            match compRes with
            | ValueOrCancelled.Value res ->
                Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicFunctions.Dispose resource

                match res with
                | Choice1Of2 r -> ValueOrCancelled.Value r
                | Choice2Of2 err -> raise err
            | ValueOrCancelled.Cancelled err1 -> ValueOrCancelled.Cancelled err1)

    member inline _.TryFinally(comp, [<InlineIfLambda>] compensation) =
        Cancellable(fun ct ->
#if !FSHARPCORE_USE_PACKAGE
            __debugPoint ""
#endif

            let compRes =
                try
                    match Cancellable.run ct comp with
                    | ValueOrCancelled.Value res -> ValueOrCancelled.Value(Choice1Of2 res)
                    | ValueOrCancelled.Cancelled exn -> ValueOrCancelled.Cancelled exn
                with err ->
                    ValueOrCancelled.Value(Choice2Of2 err)

            match compRes with
            | ValueOrCancelled.Value res ->
                compensation ()

                match res with
                | Choice1Of2 r -> ValueOrCancelled.Value r
                | Choice2Of2 err -> raise err
            | ValueOrCancelled.Cancelled err1 -> ValueOrCancelled.Cancelled err1)

    member inline _.Return v =
        Cancellable(fun _ -> ValueOrCancelled.Value v)

    member inline _.ReturnFrom(v: Cancellable<'T>) = v

    member inline _.Zero() =
        Cancellable(fun _ -> ValueOrCancelled.Value())
*)
[<AutoOpen>]
module CancellableAutoOpens =
    let cancellable = CancellableBuilder()
