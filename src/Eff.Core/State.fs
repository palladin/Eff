namespace Eff.Core

// Basic state operations
type Put<'S, 'Ans>(v : 'S, k : unit -> 'Ans) =
    inherit Effect()
    member self.Value = v
    member self.K = k

type Get<'S, 'Ans>(k : 'S -> 'Ans) =
    inherit Effect()
    member self.K = k


module State = 


    let put (v : 'S) : Eff<unit, 'Ans> =
        Eff (fun (k, exk) ->  exk <| new Put<'S,'Ans>(v, k))

    let get () : Eff<'S, 'Ans> = 
        Eff (fun (k, exk) ->  exk <| new Get<'S,'Ans>(k))