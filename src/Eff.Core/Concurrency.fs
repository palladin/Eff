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
    let rec sequentialHandler<'U, 'A when 'U :> Concur> 
        : Eff<'U, 'A> -> Eff<'U, 'A> = 
        fun eff -> 
            let rec loop : Queue<unit -> Effect> -> ('T -> Effect) -> Effect -> Effect =
                fun queue k effect -> 
                    match effect with
                    | :? Fork as fork -> 
                        queue.Enqueue(fork.K)
                        let effect' = fork.Effect
                        loop queue k effect'
                    | :? Yield as yield' ->
                        queue.Enqueue(yield'.K)
                        let effect' = queue.Dequeue() ()
                        loop queue k effect'
                    | :? Done<'T> as done' -> 
                        if queue.Count <> 0 then
                            let effect' = queue.Dequeue() ()
                            loop queue k effect'
                        else k done'.Value
                    | _ -> 
                        effect.UnPack {
                            new Lambda with
                                member self.Invoke<'X> (k' : 'X -> Effect) = 
                                    fun x -> loop queue k (k' x)
                        }
            let (Eff effK) = eff
            let effect = effK done'
            Eff (fun k -> loop (new Queue<_>()) k effect)
//
//    let threadPoolHandler (eff : Eff<'T, Effect>) : Eff<'T, Effect> =
//            let rec loop (resultK : 'T -> Effect) (exK : exn -> Effect) (effK : Effect -> (Effect -> EffCont<Effect> -> Effect) -> Effect) (effect : Effect) (k : Effect -> EffCont<Effect> -> Effect) : Effect = 
//                match effect with
//                | :? Fork<Effect> as fork -> 
//                    let (Eff cont) = fork.Value
//                    ThreadPool.QueueUserWorkItem(fun _ -> 
//                        let effect' = cont (done', exK, EffCont (loop resultK exK effK))
//                        loop resultK exK effK effect' k |> ignore) |> ignore
//                    loop resultK exK effK (fork.K ()) k
//                | :? Yield<Effect> as yield' ->
//                    loop resultK exK effK (yield'.K ()) k
//                | :? Done<'T> as done' -> 
//                    let effect' = resultK done'.Value
//                    effK effect' (fun effect' (EffCont effK') -> k effect' (EffCont (loop resultK exK effK')))
//                | _ ->
//                    effK effect (fun effect' (EffCont effK') -> k effect' (EffCont (loop resultK exK effK')))
//            Eff (fun (k, exK, EffCont effK) -> 
//                        let (Eff cont) = eff 
//                        let effK' = loop k exK effK
//                        let effect = cont (done', exK, EffCont effK')
//                        runEffCont effect (EffCont effK')) 
//

       