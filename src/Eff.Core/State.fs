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

    // state helper functions
    let put (v : 'S) : Eff<unit, 'Ans> =
        Eff (fun (k, exk) ->  exk <| new Put<'S,'Ans>(v, k))

    let get () : Eff<'S, 'Ans> = 
        Eff (fun (k, exk) ->  exk <| new Get<'S,'Ans>(k))


    // state effect handlers
    let pureState<'T, 'S, 'Ans> (eff' : Eff<'T, 'S -> 'Ans>) : Eff<'T, 'S -> 'Ans> = 
        eff {
            try
                return! eff'
            with 
                | :? Get<'S, 'S -> 'Ans> as get -> return! Eff (fun _ s -> get.K s s)
                | :? Put<'S, 'S -> 'Ans> as put -> return! Eff (fun _ _ -> put.K () put.Value)
        }

    let refState<'T, 'S, 'Ans> (eff' : Eff<'T, 'Ans>) : Eff<'T, 'Ans> = 
        eff {
            let stateRef = ref Unchecked.defaultof<'S>
            try
                return! eff'
            with 
                | :? Get<'S, 'Ans> as get -> return! Eff (fun _ -> get.K !stateRef)
                | :? Put<'S, 'Ans> as put -> return! Eff (fun _ -> stateRef := put.Value; put.K () )
        }

    let collectStates<'T, 'S, 'Ans> (eff' : Eff<'T, 'S -> ('Ans * 'S list)>) : Eff<'T, 'S -> ('Ans * 'S list)> = 
        eff {
            try
                return! eff'
            with 
                | :? Get<'S, 'S -> ('Ans * 'S list)> as get -> 
                    return! Eff (fun _ -> (fun s -> get.K s s))
                | :? Put<'S, 'S -> ('Ans * 'S list)> as put -> 
                    return! Eff (fun _ -> (fun _ ->
                                                let x = put.Value 
                                                let (v, xs) = put.K () x
                                                (v, x :: xs)))
        }
    