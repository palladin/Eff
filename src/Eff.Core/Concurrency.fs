namespace Eff.Core

open System.Collections.Generic
open System.Threading

// Basic concurrency operation

type Concur = inherit Effect

type Fork(effect : Effect, k : unit -> Effect) =
    interface Concur with
        member self.UnPack(lambda : Lambda) : Effect =
            new Fork(effect, lambda.Invoke<unit> k) :> _
    member self.Effect = effect
    member self.K = k

type Yield(k : unit -> Effect) =
    interface Concur with
        member self.UnPack(lambda : Lambda) : Effect =
            new Yield(lambda.Invoke<unit> k) :> _
    member self.K = k


module Concurrent = 

    // concurrency helper functions
    let fork<'U when 'U :> Concur> : Eff<'U, unit> -> Eff<'U, unit> =  
        fun eff -> shift (fun k -> let (Eff cont) = eff in new Fork(cont k, k) :> _)

    let yield'<'U when 'U :> Concur>() : Eff<'U, unit> = 
        shift (fun k -> new Yield(k) :> _)


    // concurrency effect handlers
    let rec sequentialHandler<'U, 'S, 'A when 'U :> Concur and 'U :> Log<'S>> 
        : Eff<'U, 'A> -> Eff<'U, 'A * list<'S>> = 
        fun eff -> 
            let rec loop : list<'S> -> Queue<unit -> Effect> -> ('A * list<'S> -> Effect) -> Effect -> Effect =
                fun s queue k effect -> 
                    match effect with
                    | :? Fork as fork -> 
                        queue.Enqueue(fork.K)
                        let effect' = fork.Effect
                        loop s queue k effect'
                    | :? Yield as yield' ->
                        queue.Enqueue(yield'.K)
                        let effect' = queue.Dequeue() ()
                        loop s queue k effect'
                    | :? LogEntry<'S> as log -> 
                        loop (log.Value :: s) queue k (log.K ()) 
                    | :? Done<'A> as done' -> 
                        if queue.Count <> 0 then
                            let effect' = queue.Dequeue() ()
                            loop s queue k effect'
                        else k (done'.Value, List.rev s)
                    | _ -> 
                        effect.UnPack {
                            new Lambda with
                                member self.Invoke<'X> (k' : 'X -> Effect) = 
                                    fun x -> loop s queue k (k' x)
                        }
            let (Eff effK) = eff
            let effect = effK done'
            Eff (fun k -> loop [] (new Queue<_>()) k effect)

    let rec threadPoolHandler<'U, 'S, 'A when 'U :> Concur and 'U :> Log<'S>> 
        : Eff<'U, 'A> -> Eff<'U, 'A> = 
        fun eff -> 
            let rec loop : ('T -> Effect) -> Effect -> Effect = 
                fun k effect ->
                    match effect with
                    | :? Fork as fork -> 
                      ThreadPool.QueueUserWorkItem(fun _ -> loop k fork.Effect |> ignore) |> ignore
                      loop k (fork.K ())
                    | :? Yield as yield' ->
                        loop k (yield'.K ())
                    | :? LogEntry<'S> as log -> 
                        printfn "Log: %A" log.Value; loop k (log.K ())
                    | :? Done<'T> as done' -> k done'.Value
                    | _ -> 
                        effect.UnPack {
                            new Lambda with
                                member self.Invoke<'X> (k' : 'X -> Effect) = 
                                    fun x -> loop k (k' x)
                        }
            let (Eff effK) = eff
            let effect = effK done'
            Eff (fun k -> loop k effect)

       