namespace Eff.Core

open System

// Core types and Builder

type Eff<'T, 'R> = Eff of ((('T -> 'R) * (exn -> 'R)) -> 'R)

// Annotation type for Effects
type Effect() =
    inherit Exception()

// Basic builder 
type EffBuilder() = 
    member self.Return (x : 'T) : Eff<'T, 'R> = Eff (fun (k, _) -> k x)
    member self.ReturnFrom (eff : Eff<'T, 'R>) : Eff<'T, 'R> = eff
    member self.Bind (eff : Eff<'A, 'R>, f : 'A -> Eff<'B, 'R>) : Eff<'B, 'R> =
        Eff (fun (k, exk) -> let (Eff cont) = eff in cont ((fun v -> let (Eff cont') = f v in cont' (k, exk)), exk))
    member self.TryWith (eff : Eff<'T, 'R>, f : exn -> Eff<'T, 'R>) : Eff<'T, 'R> =
        Eff (fun (k, exk) -> 
                let (Eff cont) = eff
                cont (k, (fun ex -> 
                    match (try Choice1Of2 (f ex) with ex -> Choice2Of2 ex) with
                    | Choice1Of2 (Eff cont') -> cont' (k, exk)
                    | Choice2Of2 ex -> exk ex)))
     member self.Delay (f : unit -> Eff<'T, 'R>) : Eff<'T, 'R> = 
        Eff (fun (k, exk) -> let (Eff cont) = f () in cont (k, exk))


[<AutoOpen>]
module Eff = 

    let run (return' : 'T -> 'R) (eff : Eff<'T, 'R>) = let (Eff cont) = eff in cont (return', fun ex -> raise ex)

    let eff = new EffBuilder()
    
