namespace Eff.Core

type NonDetEffect = 
    inherit Effect
    abstract Invoke<'R> : NonDetUnPack<'R> -> 'R
and NonDetUnPack<'R> =
    abstract Invoke : (unit -> Effect) -> 'R 
    abstract Invoke<'T> : 'T * 'T * ('T -> Effect) -> 'R

//// Basic non-determinism operations
and Choose<'T>(first : 'T, second : 'T, k : 'T -> Effect) =
    interface NonDetEffect with
        override self.Invoke unpack = 
            unpack.Invoke<'T>(first, second, k)
        override self.UnPack(lambda : Lambda) : Effect =
            new Choose<'T>(first, second, lambda.Invoke<'T> k) :> _
and Fail(k : unit -> Effect) =
    interface NonDetEffect with
        override self.Invoke unpack = 
            unpack.Invoke k
        override self.UnPack(lambda : Lambda) : Effect =
            new Fail(lambda.Invoke<unit> k) :> _
    
module NonDet = 
    
    // non-determinism helper functions
    let choose<'T, 'U when 'U :> NonDetEffect> (first : 'T, second : 'T) : Eff<'U, 'T> = 
        shift (fun k -> new Choose<'T>(first, second, k) :> _)

    let fail<'U when 'U :> NonDetEffect> () : Eff<'U, unit> = 
        shift (fun k -> new Fail(k) :> _)
//
//    // non-determinism effect handlers
    let nonDetHandler<'U, 'A when 'U :> NonDetEffect> 
        : Eff<'U, 'A> -> Eff<'U, list<'A>> = 
        fun eff ->
            let rec loop : (list<'A> -> Effect) -> Effect -> Effect =
                fun k effect -> 
                    match effect with
                    | :? NonDetEffect as nonDet -> 
                        let results = nonDet.Invoke <| { new NonDetUnPack<Effect> with
                                                            member self.Invoke k' = 
                                                                k []
                                                            member self.Invoke<'C>(first : 'C, second : 'C, k' : 'C -> Effect) =
                                                                loop (fun seqs -> loop (fun seqs' -> k <| List.append seqs seqs') (k' second)) (k' first) }
                        results
                    | :? Done<'A> as done' -> k [done'.Value]
                    | _ -> effect.UnPack {
                        new Lambda with
                            member self.Invoke<'X> (k' : 'X -> Effect) = 
                                fun x -> loop k (k' x)
                    }
            let (Eff effK) = eff
            let effect = effK done'
            Eff (fun k -> loop k effect)
//    let nonDetHandler (eff : Eff<'T, Effect>) : Eff<list<'T>, Effect> =
//        let rec loop (resultK : list<'T> -> Effect) (effK : Effect -> (Effect -> EffCont<Effect> -> Effect) -> Effect) (effect : Effect) (k : Effect -> EffCont<Effect> -> Effect) : Effect = 
//            match effect with
//            | :? NonDetEffect<Effect> as nonDet -> 
//                let results = nonDet.Invoke <| { new NonDetUnPack<Effect, Effect> with
//                                                    member self.Invoke k' = 
//                                                        resultK []
//                                                    member self.Invoke<'C>(first : 'C, second : 'C, k' : 'C -> Effect) =
//                                                        loop (fun seqs -> loop (fun seqs' -> resultK <| List.append seqs seqs') effK (k' second) k) effK (k' first) k }
//                results
//            | :? Done<'T> as done' -> 
//                let effect' = resultK [done'.Value]
//                effK effect' (fun effect' (EffCont effK') -> k effect' (EffCont (loop resultK effK')))
//            | _ -> 
//                effK effect (fun effect' (EffCont effK') -> k effect' (EffCont (loop resultK effK')))
//        Eff (fun (k, exK, EffCont effK) -> 
//                let (Eff cont) = eff 
//                let effK' = loop k effK
//                let effect = cont (done', exK, EffCont effK')
//                runEffCont effect (EffCont effK'))
