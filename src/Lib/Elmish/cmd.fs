(**
Cmd
---------
Core abstractions for dispatching messages in Elmish.

*)

namespace Elmish

open System

/// Dispatch - feed new message into the processing loop
type Dispatch<'msg> = 'msg -> Async<unit>

/// Subscription - return immediately, but may schedule dispatch of a message at any time
type Sub<'msg, 'env> = Dispatch<'msg> -> 'env -> Async<unit>

/// Cmd - container for subscriptions that may produce messages
type Cmd<'msg, 'env> = Sub<'msg, 'env> list

/// Cmd module for creating and manipulating commands
[<RequireQualifiedAccess>]
module Cmd =
    /// Execute the commands using the supplied dispatcher
    let internal exec (dispatch:Dispatch<'msg>) (env: 'env) (cmd:Cmd<'msg, 'env>) = async {
            for sub in cmd do
                do! sub dispatch env
        }

    /// None - no commands, also known as `[]`
    let none : Cmd<'msg, 'env> =
        []

    /// When emitting the message, map to another type
    let map (f: 'a -> 'msg) (cmd: Cmd<'a, 'env>) : Cmd<'msg, 'env> =
        cmd |> List.map (fun g -> (fun dispatch -> f >> dispatch) >> g)

    /// Aggregate multiple commands
    let batch (cmds: #seq<Cmd<'msg, 'env>>) : Cmd<'msg, 'env> =
        cmds |> List.concat

    /// Command to call the subscriber
    let ofSub (sub: Sub<'msg, 'env>) : Cmd<'msg, 'env> =
        [sub]

//    module OfFunc =
//        /// Command to evaluate a simple function and map the result
//        /// into success or error (of exception)
//        let either (task: 'a -> _) (arg: 'a) (ofSuccess: _ -> 'msg) (ofError: _ -> 'msg) : Cmd<'msg, 'env> =
//            let bind dispatch _ =
//                try
//                    task arg
//                    |> (ofSuccess >> dispatch)
//                with x ->
//                    x |> (ofError >> dispatch)
//                async.Return ()
//            [bind]
//
//        /// Command to evaluate a simple function and map the success to a message
//        /// discarding any possible error
//        let perform (task: 'a -> _) (arg: 'a) (ofSuccess: _ -> 'msg) : Cmd<'msg, 'env> =
//            let bind dispatch _ =
//                try
//                    task arg
//                    |> (ofSuccess >> dispatch)
//                with x ->
//                    ()
//            [bind]
//
//        /// Command to evaluate a simple function and map the error (in case of exception)
//        let attempt (task: 'a -> unit) (arg: 'a) (ofError: _ -> 'msg) : Cmd<'msg, 'env> =
//            let bind dispatch _ =
//                try
//                    task arg
//                with x ->
//                    x |> (ofError >> dispatch)
//            [bind]
//
//        /// Command to issue a specific message
//        let result (msg:'msg) : Cmd<'msg, 'env> =
//            [fun dispatch _ -> dispatch msg]

//    module OfAsync =
//        /// Command that will evaluate an async block and map the result
//        /// into success or error (of exception)
//        let either (task: 'a -> Async<_>)
//                   (arg: 'a)
//                   (ofSuccess: _ -> 'msg)
//                   (ofError: _ -> 'msg) : Cmd<'msg, 'env> =
//            let bind dispatch _ =
//                async {
//                    let! r = task arg |> Async.Catch
//                    dispatch (match r with
//                             | Choice1Of2 x -> ofSuccess x
//                             | Choice2Of2 x -> ofError x)
//                }
//            [fun dispatch _ -> bind dispatch () |> Async.StartImmediate]
//
//        /// Command that will evaluate an async block and map the success
//        let perform (task: 'a -> Async<_>)
//                    (arg: 'a)
//                    (ofSuccess: _ -> 'msg) =
//            let bind dispatch _ =
//                async {
//                    let! r = task arg |> Async.Catch
//                    match r with
//                    | Choice1Of2 x -> dispatch (ofSuccess x)
//                    | _ -> ()
//                }
//            [fun d _ -> bind d () |> Async.StartImmediate]
//
//        /// Command that will evaluate an async block and map the error (of exception)
//        let attempt (task: 'a -> Async<_>)
//                    (arg: 'a)
//                    (ofError: _ -> 'msg) =
//            let bind dispatch =
//                async {
//                    let! r = task arg |> Async.Catch
//                    match r with
//                    | Choice2Of2 x -> dispatch (ofError x)
//                    | _ -> ()
//                }
//            [fun d _ -> bind d |> Async.StartImmediate]
//
//        /// Command that will evaluate an async block and map the success
//        let result (task: Async<_>)
//                   (ofSuccess: _ -> 'msg) =
//            let bind dispatch =
//                async {
//                    let! r = task |> Async.Catch
//                    match r with
//                    | Choice1Of2 x -> dispatch (ofSuccess x)
//                    | _ -> ()
//                }
//            [fun d _ -> bind d |> Async.StartImmediate]

#if FABLE_COMPILER
    module OfPromise =
        /// Command to call `promise` block and map the results
        let either (task: 'a -> Fable.Core.JS.Promise<_>)
                   (arg:'a)
                   (ofSuccess: _ -> 'msg)
                   (ofError: #exn -> 'msg) : Cmd<'msg> =
            let bind dispatch =
                (task arg)
                    .``then``(ofSuccess >> dispatch)
                    .catch(unbox >> ofError >> dispatch)
                    |> ignore
            [bind]

        /// Command to call `promise` block and map the success
        let perform (task: 'a -> Fable.Core.JS.Promise<_>)
                   (arg:'a)
                   (ofSuccess: _ -> 'msg) =
            let bind dispatch =
                (task arg)
                    .``then``(ofSuccess >> dispatch)
                    |> ignore
            [bind]

        /// Command to call `promise` block and map the error
        let attempt (task: 'a -> Fable.Core.JS.Promise<_>)
                    (arg:'a)
                    (ofError: #exn -> 'msg) : Cmd<'msg> =
            let bind dispatch =
                (task arg)
                    .catch(unbox >> ofError >> dispatch)
                    |> ignore
            [bind]

        /// Command to map `promise` the result
        let result (task: Fable.Core.JS.Promise<_>)
                   (ofSuccess: _ -> 'msg) =
            let bind dispatch =
                task.``then``(ofSuccess >> dispatch)
                |> ignore
            [bind]

    [<Obsolete("Use `OfPromise.either` instead")>]
    let inline ofPromise (task: 'a -> Fable.Core.JS.Promise<_>)
                         (arg:'a)
                         (ofSuccess: _ -> 'msg)
                         (ofError: _ -> 'msg) : Cmd<'msg> =
        OfPromise.either task arg ofSuccess ofError                     
#else
    open System.Threading.Tasks
//    module OfTask =
//        /// Command to call a task and map the results
//        let inline either (task: 'a -> Task<_>)
//                          (arg:'a)
//                          (ofSuccess: _ -> 'msg)
//                          (ofError: _ -> 'msg) : Cmd<'msg, 'env> =
//            OfAsync.either (task >> Async.AwaitTask) arg ofSuccess ofError
//
//        /// Command to call a task and map the success
//        let inline perform (task: 'a -> Task<_>)
//                           (arg:'a)
//                           (ofSuccess: _ -> 'msg) : Cmd<'msg, 'env> =
//            OfAsync.perform (task >> Async.AwaitTask) arg ofSuccess
//
//        /// Command to call a task and map the error
//        let inline attempt (task: 'a -> Task<_>)
//                           (arg:'a)
//                           (ofError: _ -> 'msg) : Cmd<'msg, 'env> =
//            OfAsync.attempt (task >> Async.AwaitTask) arg ofError
//
//        /// Command and map the task success
//        let inline result (task: Task<_>)
//                          (ofSuccess: _ -> 'msg) : Cmd<'msg, 'env> =
//            OfAsync.result (task |> Async.AwaitTask) ofSuccess
//
//    [<Obsolete("Use OfTask.either instead")>]
//    let inline ofTask (task: 'a -> Task<_>)
//                      (arg:'a)
//                      (ofSuccess: _ -> 'msg)
//                      (ofError: _ -> 'msg) : Cmd<'msg, 'env> =
//        OfTask.either task arg ofSuccess ofError
#endif
//
//    [<Obsolete("Use `OfFunc.result` instead")>]
//    let inline ofMsg (msg:'msg) : Cmd<'msg, 'env> =
//        OfFunc.result msg
//
//    [<Obsolete("Use `OfAsync.either` instead")>]
//    let inline ofAsync (task: 'a -> Async<_>)
//                       (arg: 'a)
//                       (ofSuccess: _ -> 'msg)
//                       (ofError: _ -> 'msg) : Cmd<'msg, 'env> =
//        OfAsync.either task arg ofSuccess ofError
//
//    [<Obsolete("Use `OfFunc.either` instead")>]
//    let inline ofFunc (task: 'a -> _) (arg: 'a) (ofSuccess: _ -> 'msg) (ofError: _ -> 'msg) : Cmd<'msg, 'env> =
//        OfFunc.either task arg ofSuccess ofError
//
//    [<Obsolete("Use `OfFunc.perform` instead")>]
//    let inline performFunc (task: 'a -> _) (arg: 'a) (ofSuccess: _ -> 'msg) : Cmd<'msg, 'env> =
//        OfFunc.perform task arg ofSuccess
//
//    [<Obsolete("Use `OfFunc.attempt` instead")>]
//    let attemptFunc (task: 'a -> unit) (arg: 'a) (ofError: _ -> 'msg) : Cmd<'msg, 'env> =
//        OfFunc.attempt task arg ofError
