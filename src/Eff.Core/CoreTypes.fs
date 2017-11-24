namespace Eff.Core

open System

// Core types and Builder
type Eff<'U, 'A when 'U :> Effect> = Eff of (('A -> Effect) -> Effect)
// Annotation type for Effects
and Effect = 
    abstract UnPack : Lambda -> Effect 
and Lambda =
        abstract Invoke<'X> : ('X -> Effect) -> ('X -> Effect)
and Done<'A>(v : 'A) =
    member self.Value = v
    interface Effect with
        member self.UnPack(_ : Lambda) : Effect =
                new Done<'A>(v) :> _

// Basic builder 
type EffBuilder() = 
    member self.Return<'U, 'A when 'U :> Effect> (v : 'A) : Eff<'U, 'A> = 
        Eff (fun k -> k v)
    member self.ReturnFrom (eff : Eff<'U, 'A>) = eff
    member self.Combine (first : Eff<'U, 'A>, second : Eff<'U, unit>) : Eff<'U, unit> = 
        self.Bind(first, fun _ -> second)
    member self.Zero () : Eff<'U, unit> = 
        Eff (fun k -> k ())
    member self.Bind<'U, 'A, 'B when 'U :> Effect>(eff : Eff<'U, 'A>, f : 'A -> Eff<'U, 'B>) : Eff<'U, 'B> = 
        Eff (fun k -> let (Eff effK) = eff in effK (fun v -> let (Eff effK') = f v in effK' k))    
     member self.Delay (f : unit -> Eff<'U, 'A>) : Eff<'U, 'A> = 
        Eff (fun k -> let (Eff cont) = f () in cont k)


[<AutoOpen>]
module Eff = 

    let done' (v : 'A) : Effect = 
        new Done<'A>(v) :> _ 
    let shift (f : ('A -> Effect) -> Effect) : Eff<'U, 'A> = 
        Eff (fun k -> f k)

    let rec run<'U, 'A when 'U :> Effect> : Eff<'U, 'A> -> 'A = 
        fun eff ->
            let (Eff effK) = eff
            let effect = effK done'
            match effect with
            | :? Done<'A> as done' -> done'.Value
            | _ -> failwithf "Unhandled effect %A" effect

    let eff = new EffBuilder()
    
