
#load "CoreTypes.fs"
#load "State.fs"
#load "NonDet.fs"
open Eff.Core
open Eff.Core.State
open Eff.Core.NonDet

// State examples

let test () : Eff<int, Effect> = 
    eff {
        let! x = get ()
        do! put (x + 1)
        let! y = get ()
        do! put (y + y)
        return! get ()
    } 

let stateHandler (s : 'S) (eff : Eff<'T, Effect>) : Eff<'T * 'S, Effect> =
    let rec loop (s : 'S) (effect : Effect) = 
        match effect with
        | :? Get<'S, Effect> as get -> loop s (get.K s) 
        | :? Put<'S, Effect> as put -> loop put.Value (put.K ())
        | :? Done<'T> as done' -> (done'.Value, s)
        | _ -> failwith "Unhandled effect"
    Eff (fun (k, exK) -> 
                let (Eff cont) = eff 
                let effect = cont (done', exK)
                k <| loop s effect) 

test () |> stateHandler 1 |> run // (4, 4)


// Non-determinism examples
let nonDetTest () : Eff<int * string, Effect> = 
    eff {
        let! x = choose (1, 2)
        let! y = choose ("1", "2")
        return (x, y)
    }

let nonDetHandler (eff : Eff<'T, Effect>) : Eff<seq<'T>, Effect> =
    let rec loop (effect : Effect) : seq<'T> = 
        match effect with
        | :? NonDetEffect<Effect> as nonDet -> 
            let results = nonDet.Invoke <| { new NonDetUnPack<Effect, seq<'T>> with
                                                member self.Invoke k = loop <| k ()
                                                member self.Invoke<'C>(first : 'C, second : 'C, k : 'C -> Effect) =
                                                    seq { yield! loop (k first); yield! loop (k second) }  }
            results
        | :? Done<'T> as done' -> Seq.singleton done'.Value
        | _ -> failwith "Unhandled effect"
    Eff (fun (k, exK) -> 
            let (Eff cont) = eff 
            let effect = cont (done', exK)
            k <| loop effect)
    
nonDetTest () |> nonDetHandler |> run // seq [(1, "1"); (1, "2"); (2, "1"); (2, "2")]
