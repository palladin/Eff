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
    let nonDetHandler (eff : Eff<'T, Effect>) : Eff<list<'T>, Effect> =
        let rec loop (resultK : list<'T> -> Effect) (effK : Effect -> (Effect -> EffCont<Effect> -> Effect) -> Effect) (effect : Effect) (k : Effect -> EffCont<Effect> -> Effect) : Effect = 
            match effect with
            | :? NonDetEffect<Effect> as nonDet -> 
                let results = nonDet.Invoke <| { new NonDetUnPack<Effect, Effect> with
                                                    member self.Invoke k' = 
                                                        resultK []
                                                    member self.Invoke<'C>(first : 'C, second : 'C, k' : 'C -> Effect) =
                                                        loop (fun seqs -> loop (fun seqs' -> resultK <| List.append seqs seqs') effK (k' second) k) effK (k' first) k }
                results
            | :? Done<'T> as done' -> 
                let effect' = resultK [done'.Value]
                effK effect' (fun effect' (EffCont effK') -> k effect' (EffCont (loop resultK effK')))
            | _ -> 
                effK effect (fun effect' (EffCont effK') -> k effect' (EffCont (loop resultK effK')))
        Eff (fun (k, exK, EffCont effK) -> 
                let (Eff cont) = eff 
                let effK' = loop k effK
                let effect = cont (done', exK, EffCont effK')
                runEffCont effect (EffCont effK'))
