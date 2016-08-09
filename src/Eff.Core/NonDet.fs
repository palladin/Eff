namespace Eff.Core

type NonDetEffect<'Eff when 'Eff :> Effect> = 
    inherit Effect
    abstract Invoke<'R> : NonDetUnPack<'Eff, 'R> -> 'R
and NonDetUnPack<'Eff, 'R> =
    abstract Invoke : (unit -> 'Eff) -> 'R 
    abstract Invoke<'T> : 'T * 'T * ('T -> 'Eff) -> 'R

// Basic non-determinism operations
and Choose<'T, 'Eff when 'Eff :> Effect>(first : 'T, second : 'T, k : 'T -> 'Eff) =
    interface NonDetEffect<'Eff> with
        override self.Invoke unpack = 
            unpack.Invoke<'T>(first, second, k)

and Fail<'Eff when 'Eff :> Effect>(k : unit -> 'Eff) =
    interface NonDetEffect<'Eff> with
        override self.Invoke unpack = 
            unpack.Invoke k
    
module NonDet = 
    
    // non-determinism helper functions
    let choose (first : 'T, second : 'T) = 
        shift (fun k -> new Choose<'T, Effect>(first, second, k) :> _)

    let fail () = 
        shift (fun k -> new Fail<Effect>(k) :> _)

    // non-determinism effect handlers
    