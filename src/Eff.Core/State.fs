namespace Eff.Core

type StateEffect<'S> = 
    inherit Effect

// Basic state operations
type Put<'S, 'Eff when 'Eff :> Effect>(v : 'S, k : unit -> 'Eff) =
    interface StateEffect<'S>
    member self.Value = v
    member self.K = k

type Get<'S, 'Eff when 'Eff :> Effect>(k : 'S -> 'Eff) =
    interface StateEffect<'S>
    member self.K = k


module State = 

    // state helper functions
    let put (v : 'S) : Eff<unit, Effect> =
        shift (fun k -> new Put<'S, Effect>(v, k) :> _)

    let get () : Eff<'S, Effect> = 
        shift (fun k -> new Get<'S, Effect>(k) :> _)


    // state effect handlers
    let stateHandler (s : 'S) (eff : Eff<'T, Effect>) : Eff<'T * 'S, Effect> =
        let rec loop (s : 'S) (resultK : 'T * 'S -> Effect) (effK : Effect -> (Effect -> EffCont<Effect> -> Effect) -> Effect) (effect : Effect) (k : Effect -> EffCont<Effect> -> Effect) : Effect = 
            match effect with
            | :? Get<'S, Effect> as get -> loop s resultK effK (get.K s) k
            | :? Put<'S, Effect> as put -> loop put.Value resultK effK (put.K ()) k
            | :? Done<'T> as done' -> 
                let effect' = resultK (done'.Value, s)
                effK effect' (fun effect' (EffCont effK') -> k effect' (EffCont (loop s resultK effK')))
            | _ ->
                effK effect (fun effect' (EffCont effK') -> k effect' (EffCont (loop s resultK effK')))
        Eff (fun (k, exK, EffCont effK) -> 
                    let (Eff cont) = eff 
                    let effK' = loop s k effK
                    let effect = cont (done', exK, EffCont effK')
                    runEffCont effect (EffCont effK')) 

