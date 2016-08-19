namespace Eff.Core

open System.Collections.Generic
open System.Threading

// Basic concurrency operation
type Fork<'Eff when 'Eff :> Effect>(value : Eff<unit, 'Eff>, k : unit -> 'Eff) =
    interface Effect
    member self.Value = value
    member self.K = k

type Yield<'Eff when 'Eff :> Effect>(k : unit -> 'Eff) =
    interface Effect
    member self.K = k


module Concurrent = 

    // concurrency helper functions
    let fork (eff : Eff<unit, Effect>) : Eff<unit, Effect> =
        shift (fun k -> new Fork<Effect>(eff, k) :> _)

    let yield' () : Eff<unit, Effect> =
        shift (fun k -> new Yield<Effect>(k) :> _)


    // concurrency effect handlers
    let sequentialHandler (eff : Eff<'T, Effect>) : Eff<'T, Effect> =
        let rec loop (queue : Queue<unit -> Effect>) (resultK : 'T -> Effect) (exK : exn -> Effect) (effK : Effect -> (Effect -> EffCont<Effect> -> Effect) -> Effect) (effect : Effect) (k : Effect -> EffCont<Effect> -> Effect) : Effect = 
            match effect with
            | :? Fork<Effect> as fork -> 
                queue.Enqueue(fork.K)
                let (Eff cont) = fork.Value
                let effect' = cont (done', exK, EffCont (loop queue resultK exK effK))
                loop queue resultK exK effK effect' k
            | :? Yield<Effect> as yield' ->
                queue.Enqueue(yield'.K)
                let effect' = queue.Dequeue() ()
                loop queue resultK exK effK effect' k
            | :? Done<'T> as done' -> 
                if queue.Count <> 0 then
                    let effect' = queue.Dequeue() ()
                    loop queue resultK exK effK effect' k                    
                else
                    let effect' = resultK done'.Value
                    effK effect' (fun effect' (EffCont effK') -> k effect' (EffCont (loop queue resultK exK effK')))
            | _ ->
                effK effect (fun effect' (EffCont effK') -> k effect' (EffCont (loop queue resultK exK effK')))
        Eff (fun (k, exK, EffCont effK) -> 
                    let (Eff cont) = eff 
                    let effK' = loop (new Queue<_>()) k exK effK
                    let effect = cont (done', exK, EffCont effK')
                    runEffCont effect (EffCont effK')) 

    let threadPoolHandler (eff : Eff<'T, Effect>) : Eff<'T, Effect> =
            let rec loop (resultK : 'T -> Effect) (exK : exn -> Effect) (effK : Effect -> (Effect -> EffCont<Effect> -> Effect) -> Effect) (effect : Effect) (k : Effect -> EffCont<Effect> -> Effect) : Effect = 
                match effect with
                | :? Fork<Effect> as fork -> 
                    let (Eff cont) = fork.Value
                    ThreadPool.QueueUserWorkItem(fun _ -> 
                        let effect' = cont (done', exK, EffCont (loop resultK exK effK))
                        loop resultK exK effK effect' k |> ignore) |> ignore
                    loop resultK exK effK (fork.K ()) k
                | :? Yield<Effect> as yield' ->
                    loop resultK exK effK (yield'.K ()) k
                | :? Done<'T> as done' -> 
                    let effect' = resultK done'.Value
                    effK effect' (fun effect' (EffCont effK') -> k effect' (EffCont (loop resultK exK effK')))
                | _ ->
                    effK effect (fun effect' (EffCont effK') -> k effect' (EffCont (loop resultK exK effK')))
            Eff (fun (k, exK, EffCont effK) -> 
                        let (Eff cont) = eff 
                        let effK' = loop k exK effK
                        let effect = cont (done', exK, EffCont effK')
                        runEffCont effect (EffCont effK')) 


       