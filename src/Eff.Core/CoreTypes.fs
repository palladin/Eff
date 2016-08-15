namespace Eff.Core

open System

// Core types and Builder
type EffCont<'Eff when 'Eff :> Effect> = EffCont of ('Eff -> ('Eff -> EffCont<'Eff> -> 'Eff) -> 'Eff)
and Eff<'T, 'Eff when 'Eff :> Effect> = Eff of ((('T -> 'Eff) * (exn -> 'Eff) * EffCont<'Eff>) -> 'Eff)

// Annotation type for Effects
and Effect = interface end
and Done<'T>(v : 'T) = 
    interface Effect
    member self.Value = v

// Basic builder 
type EffBuilder() = 
    member self.Return (x : 'T) : Eff<'T, 'Eff> = Eff (fun (k, _, _) -> k x)
    member self.ReturnFrom (eff : Eff<'T, 'Eff>) : Eff<'T, 'Eff> = eff
    member self.Bind (eff : Eff<'A, 'Eff>, f : 'A -> Eff<'B, 'Eff>) : Eff<'B, 'Eff> =
        Eff (fun (k, exk, effK) -> let (Eff cont) = eff in cont ((fun v -> let (Eff cont') = f v in cont' (k, exk, effK)), exk, effK))
    member self.TryWith (eff : Eff<'T, 'Eff>, f : exn -> Eff<'T, 'Eff>) : Eff<'T, 'Eff> =
        Eff (fun (k, exk, effK) -> 
                let (Eff cont) = eff
                cont (k, (fun ex -> 
                    match (try Choice1Of2 (f ex) with ex -> Choice2Of2 ex) with
                    | Choice1Of2 (Eff cont') -> cont' (k, exk, effK)
                    | Choice2Of2 ex -> exk ex), effK))
     member self.Delay (f : unit -> Eff<'T, 'Eff>) : Eff<'T, 'Eff> = 
        Eff (fun (k, exk, effK) -> let (Eff cont) = f () in cont (k, exk, effK))


[<AutoOpen>]
module Eff = 

    let done' (v : 'T) : Effect = new Done<'T>(v) :> _ 
    let shift (f : ('T -> 'Eff) -> 'Eff) : Eff<'T, 'Eff> = Eff (fun (k, _, _) -> f k)

    let rec runEffCont (effect : Effect) (EffCont effK : EffCont<Effect>) : Effect = 
        effK effect (fun effect' effK' -> runEffCont effect' effK')

    let run (eff : Eff<'T, Effect>) : 'T =
        let rec loop (effect : Effect) (k : Effect -> EffCont<Effect> -> Effect) : Effect = 
            match effect with
            | :? Done<'T> -> effect
            | _ -> k effect (EffCont loop)
        let (Eff cont) = eff 
        let effect = cont (done', (fun ex -> raise ex), EffCont loop)
        match effect with
        | :? Done<'T> as done' -> done'.Value
        | _ -> failwithf "Unhandled effect %A" effect

    let eff = new EffBuilder()
    
