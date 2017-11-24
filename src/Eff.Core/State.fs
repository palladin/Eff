namespace Eff.Core

type State<'S> = inherit Effect
type Put<'S>(v : 'S, k : unit -> Effect) =
    interface State<'S> with
        member self.UnPack(lambda : Lambda) : Effect =
            new Put<'S>(v, lambda.Invoke<unit> k) :> _
    member self.Value = v
    member self.K = k
type Get<'S>(k : 'S -> Effect) =
    interface State<'S> with
        member self.UnPack(lambda : Lambda) : Effect =             
                new Get<'S>(lambda.Invoke<'S> k) :> _
    member self.K = k

module State = 

    // state helper functions
    let get<'U, 'S when 'U :> State<'S>>() : Eff<'U, 'S> = 
        shift (fun k -> new Get<'S>(k) :> _)
    let put<'U, 'S when 'U :> State<'S>> : 'S -> Eff<'U, unit> = fun s ->
        shift (fun k -> new Put<'S>(s, k) :> _)


    // state effect handlers
    
    let rec stateHandler<'U, 'S, 'A when 'U :> State<'S>> 
        : 'S -> Eff<'U, 'A> -> Eff<'U, 'S * 'A> = 
        fun state eff ->
            let rec loop : ('S * 'A -> Effect) -> 'S -> Effect -> Effect = fun k state effect ->
                match effect with
                | :? Get<'S> as get -> loop k state (get.K state) 
                | :? Put<'S> as put -> loop k put.Value (put.K ())
                | :? Done<'A> as done' -> k (state, done'.Value)
                | _ -> effect.UnPack {
                    new Lambda with
                        member self.Invoke<'X> (k' : 'X -> Effect) = 
                            fun x -> loop k state (k' x)
                }
            let (Eff effK) = eff
            let effect = effK done'
            Eff (fun k -> loop k state effect)

    let rec refHandler<'U, 'S, 'A when 'U :> State<'S>> 
        : 'S -> Eff<'U, 'A> -> Eff<'U, 'A> = 
        fun state eff ->
            let valueRef = ref state
            let rec loop : ('A -> Effect) -> Effect -> Effect = fun k effect ->
                match effect with
                | :? Get<'S> as get -> loop k (get.K !valueRef) 
                | :? Put<'S> as put -> valueRef := put.Value; loop k (put.K ())
                | :? Done<'A> as done' -> k done'.Value
                | _ -> effect.UnPack {
                    new Lambda with
                        member self.Invoke<'X> (k' : 'X -> Effect) = 
                            fun x -> loop k (k' x)
                }
            let (Eff effK) = eff
            let effect = effK done'
            Eff (fun k -> loop k effect)
