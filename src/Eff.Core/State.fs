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



    