
#load "CoreTypes.fs"
#load "State.fs"
open Eff.Core
open Eff.Core.State

// State examples
let test () : Eff<int, 'Ans> = 
    eff {
        let! x = get ()
        do! put (x + 1)
        let! y = get ()
        do! put (y + y)
        return! get ()
    } 
    
test () |> pureState |> run (fun x -> (fun s -> x)) |> (fun f -> f 1) // 4
test () |> refState 1 |> run (fun x -> x) // 4
test () |> collectStates |> run (fun x -> (fun s -> (x, []))) |> (fun f -> f 1) // (4, [2; 4])

// State with external handler
let test' () : Eff<int, Effect> = 
    eff {
        let! x = get' ()
        do! put' (x + 1)
        let! y = get' ()
        do! put' (y + y)
        return! get' ()
    } 

let handler (s : 'S) (eff : Eff<'T, Effect>) : ('T * 'S) =
    let rec loop (s : 'S) (effect : Effect) = 
        match effect with
        | :? Get<'S, Effect> as get -> loop s (get.K s) 
        | :? Put<'S, Effect> as put -> loop put.Value (put.K ())
        | :? Done<'T> as done' -> (done'.Value, s)
        | _ -> failwith "Unhandled effect"
    loop s (run done' eff) 

handler 1 (test' ()) // (4, 4)
