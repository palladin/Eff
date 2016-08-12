
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
    let rec loop (s : 'S) (k : 'T * 'S -> Effect) (effK : Effect -> Effect) (effect : Effect) : Effect = 
        match effect with
        | :? Get<'S, Effect> as get -> loop s k effK (get.K s) 
        | :? Put<'S, Effect> as put -> loop put.Value k effK (put.K ())
        | :? Done<'T> as done' -> k (done'.Value, s)
        | _ -> effK effect
    Eff (fun (k, exK, effK) -> 
                let (Eff cont) = eff 
                let effK' = loop s k effK
                let effect = cont (done', exK, effK')
                effK' effect) 

test () |> stateHandler 1 |> run // (4, 4)


// Non-determinism examples
let nonDetTest () : Eff<int * string, Effect> = 
    eff {
        let! x = choose (1, 2)
        let! y = choose ("1", "2")
        return (x, y)
    }

let nonDetHandler (eff : Eff<'T, Effect>) : Eff<list<'T>, Effect> =
    let rec loop (k : list<'T> -> Effect) (effK : Effect -> Effect) (effect : Effect) : Effect = 
        match effect with
        | :? NonDetEffect<Effect> as nonDet -> 
            let results = nonDet.Invoke <| { new NonDetUnPack<Effect, Effect> with
                                                member self.Invoke k' = 
                                                    k []
                                                member self.Invoke<'C>(first : 'C, second : 'C, k' : 'C -> Effect) =
                                                    loop (fun seqs -> loop (fun seqs' -> k <| List.append seqs seqs') effK (k' second)) effK (k' first) }
            results
        | :? Done<'T> as done' -> k [done'.Value]
        | _ -> effK effect
    Eff (fun (k, exK, effK) -> 
            let (Eff cont) = eff 
            let effK' = loop k effK
            let effect = cont (done', exK, effK')
            effK' effect)
    
nonDetTest () |> nonDetHandler |> run // [(1, "1"); (1, "2"); (2, "1"); (2, "2")]



// Combine state and non-determinism examples
let stateNonDetTest () : Eff<int * string, Effect> = 
    eff {
        let! n = get ()
        let! x = choose (n , n + 1)
        let! y = choose (string n, string (n + 1))
        return (x, y)
    }


let foo = stateNonDetTest () |> stateHandler 1 |> nonDetHandler |> run 