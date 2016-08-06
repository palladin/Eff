
#load "CoreTypes.fs"
#load "State.fs"
open Eff.Core
open Eff.Core.State

// State examples
let test () : Eff<int, 'Ans> = 
    eff {
        do! put 1
        let! x = get ()
        do! put (x + 1)
        let! y = get ()
        do! put (y + y)
        return! get ()
    } 
    
test () |> pureState |> run (fun x -> (fun s -> x)) |> (fun f -> f 1) // 4
test () |> refState<int, int, int> |> run (fun x -> x) // 4
test () |> collectStates |> run (fun x -> (fun s -> (x, []))) |> (fun f -> f 1) // (4, [2; 4])