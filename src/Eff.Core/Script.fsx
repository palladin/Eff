
#load "CoreTypes.fs"
#load "State.fs"
#load "Reader.fs"
#load "NonDet.fs"
#load "Log.fs"
#load "Concurrency.fs"
#load "Searcher.fs"
open Eff.Core
open Eff.Core.State
open Eff.Core.Reader
open Eff.Core.NonDet
open Eff.Core.Log
open Eff.Core.Concurrent
open Eff.Core.Searcher

// State examples
// val stateTest : unit -> Eff<'a,int> when 'a :> State<int>
let stateTest () = 
    eff {
        let! x = get ()
        do! put (x + 1)
        let! y = get ()
        do! put (y + y)
        return! get ()
    } 

stateTest () |> stateHandler 1 |> run // (4, 4)
stateTest () |> refHandler 1 |> run // 4

// Combine State and Reader effects
// val example : unit -> Eff<'a,int> when 'a :> State<int> and 'a :> Reader<int>
let example () =
    eff {
        do! put 1
        let! y = ask ()
        let! x = get ()
        return x + y
    }

type ExEffect = inherit State<int> inherit Reader<int>

example () |> stateHandler<ExEffect, _, _> 0 |> readerHandler 1 |> run // (1, 2)

// Non-determinism examples
// val nonDetTest : unit -> Eff<#NonDetEffect,(int * string)>
let nonDetTest () = 
    eff {
        let! x = choose (1, 2)
        let! y = choose ("1", "2")
        return (x, y)
    }

    
nonDetTest () |> nonDetHandler |> run // [(1, "1"); (1, "2"); (2, "1"); (2, "2")]



// Combine state and non-determinism examples
// val stateNonDetTest : unit -> Eff<'a,(int * string)> when 'a :> NonDetEffect and 'a :> State<int>
let stateNonDetTest () = 
    eff {
        let! n = get ()
        let! x = choose (n , n + 1)
        let! y = choose (string n, string (n + 1))
        return (x, y)
    }

type ExEffect' = inherit State<int> inherit NonDetEffect
stateNonDetTest () |> stateHandler<ExEffect', _, _> 1 |> nonDetHandler |> run // [((1, "1"), 1); ((1, "2"), 1); ((2, "1"), 1); ((2, "2"), 1)]


// Log effect
// val logTest : n:int -> Eff<#Log<string>,unit>
let logTest (n : int)  = 
    eff {
        do! logf "Test %d" n
        do! logf "Test %d" (n + 1)
    }

logTest 1 |> pureLogHandler |> run // ((), ["Test2"; "Test1"])
logTest 1 |> consoleLogHandler |> run // printf side-effect: Log: "Test1"\n Log: "Test2"\n

// Concurrency effect
// http://kcsrk.info/ocaml/multicore/2015/05/20/effects-multicore/
// val concurrentTest : id:int -> depth:int -> Eff<'a,unit> when 'a :> Log<string> and 'a :> Concur
let rec concurrentTest id depth =
    eff {
        do! logf "Starting number %d!" id
        if depth > 0 then 
            do! logf "Forking number %d!" (id * 2 + 1)
            do! fork <| concurrentTest (id * 2 + 1) (depth - 1)
            do! logf "Forking number %d!" (id * 2 + 2)
            do! fork <| concurrentTest (id * 2 + 2) (depth - 1)
        else 
            do! logf "Yielding in number %d!" id
            do! yield' ()
            do! logf "Resumed number %d!" id
        do! logf "Finishing number %d!" id
    }

type ExEffect'' = inherit Log<string> inherit Concur

concurrentTest 0 2 |> sequentialHandler<ExEffect'', _, _> |> run 
concurrentTest 0 2 |> threadPoolHandler<ExEffect'', _, _> |> run 

// http://math.andrej.com/2011/12/06/how-to-make-the-impossible-functionals-run-even-faster/
// val findNeighborhood : p:((int -> Eff<'U,bool>) -> Eff<'U,bool>) -> Eff<'U,bool> when 'U :> Search<int>
let findNeighborhood (p : (int -> Eff<'U, bool>) -> Eff<'U, bool>) : Eff<'U, bool> =
    p (fun n -> search n)

// val epsilon : p:((int -> Eff<'a,bool>) -> Eff<'a,bool>) -> n:int -> Eff<'a,bool> when 'a :> Search<int>
let epsilon p n = 
    eff {
        let! (_, s) = findNeighborhood p |> findNeighborhoodHandler
        let result = s |> List.tryFind (fun (n', b) -> n = n')
        match result with 
        | Some (_, b) -> return b
        | None -> return true
    }

let exists p = p (epsilon p)

let f () = epsilon (fun f -> eff { let! x = f 10 in let! y = f 11 in return x <> y }) 
[ for i in [0..20] -> run <| f () i ] 