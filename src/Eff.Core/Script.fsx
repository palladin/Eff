
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

test () |> stateHandler 1 |> run // (4, 4)

// Non-determinism examples
let nonDetTest () : Eff<int * string, Effect> = 
    eff {
        let! x = choose (1, 2)
        let! y = choose ("1", "2")
        return (x, y)
    }

    
nonDetTest () |> nonDetHandler |> run // [(1, "1"); (1, "2"); (2, "1"); (2, "2")]



// Combine state and non-determinism examples
let stateNonDetTest () : Eff<int * string, Effect> = 
    eff {
        let! n = get ()
        let! x = choose (n , n + 1)
        let! y = choose (string n, string (n + 1))
        return (x, y)
    }


stateNonDetTest () |> stateHandler 1 |> nonDetHandler |> run // [((1, "1"), 1); ((1, "2"), 1); ((2, "1"), 1); ((2, "2"), 1)]

// Unhandled effect
let unhandledEffectTest () : Eff<int, Effect> = 
    eff {
        let! n = get ()
        return n
    }

unhandledEffectTest () |> run // Unhandled effect Exception
unhandledEffectTest () |> nonDetHandler |> run // Unhandled effect Exception
unhandledEffectTest () |> stateHandler 1 |> run // (1, 1)
