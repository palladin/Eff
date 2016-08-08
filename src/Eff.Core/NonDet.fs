namespace Eff.Core

[<AbstractClass>]
type NonDetEffect<'Ans>() = 
    inherit Effect()
    abstract Invoke<'R> : NonDetUnPack<'Ans, 'R> -> 'R
and NonDetUnPack<'Ans, 'R> =
    abstract Invoke : (unit -> 'Ans) -> 'R 
    abstract Invoke<'T> : 'T * 'T * ('T -> 'Ans) -> 'R

// Basic non-determinism operations
and Choose<'T, 'Ans>(first : 'T, second : 'T, k : 'T -> 'Ans) =
    inherit NonDetEffect<'Ans>()
    override self.Invoke unpack = 
        unpack.Invoke<'T>(first, second, k)

and Fail<'Ans>(k : unit -> 'Ans) =
    inherit NonDetEffect<'Ans>()
    override self.Invoke unpack = 
        unpack.Invoke k
    
module NonDet = 
    
    // non-determinism helper functions
    let choose (first : 'T, second : 'T) = 
        Eff (fun (k, exk) ->  exk <| (new Choose<'T,'Ans>(first, second, k) :> NonDetEffect<'Ans>))

    let fail () = 
        Eff (fun (k, exk) ->  exk <| (new Fail<'Ans>(k) :> NonDetEffect<'Ans>))

    // non-determinism effect handlers
    let collectNonDet<'T, 'Ans> (eff' : Eff<'T, seq<'Ans>>) : Eff<'T, seq<'Ans>> = 
        eff {
            try
                return! eff'
            with 
                | :? NonDetEffect<seq<'Ans>> as nonDet -> 
                    let results = nonDet.Invoke <| { new NonDetUnPack<seq<'Ans>, seq<'Ans>> with
                                                        member self.Invoke k = k ()
                                                        member self.Invoke<'C>(first : 'C, second : 'C, k : 'C -> seq<'Ans>) =
                                                            seq { yield! (k first); yield! (k second) }  }
                    return! Eff (fun _ -> results)
        }