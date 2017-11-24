namespace Eff.Core

// basic operation
type Search<'S>(v : 'S, k : bool -> Effect) =
    interface Effect with
        member self.UnPack(lambda : Lambda) : Effect =
                new Search<'S>(v, lambda.Invoke<bool> k) :> _
    member self.Value = v
    member self.K = k

module Searcher = 

    // helper functions
    let search<'U, 'S when 'U :> Search<'S>> : 'S -> Eff<'U, bool> =
        fun v -> shift (fun k -> new Search<'S>(v, k) :> _)

    // effect handlers
    let rec findNeighborhoodHandler<'U when 'U :> Search<int>> 
        : Eff<'U, bool> -> Eff<'U, bool * list<int * bool>> = 
        fun eff -> 
            let rec loop : list<int * bool> -> (bool * list<int * bool> -> Effect) -> Effect -> Effect = 
                fun s k effect -> 
                    match effect with
                    | :? Search<int> as search -> 
                        let result = s |> List.tryFind (fun (n, b) -> n = search.Value)
                        match result with
                        | Some (n, b) -> loop s k (search.K b)
                        | None -> loop ((search.Value, true) :: s) (function (true, s') -> k (true, s') 
                                                                           | (false, _) -> loop ((search.Value, false) :: s) k (search.K false)) 
                                       (search.K true) 
                    | :? Done<bool> as done' -> k (done'.Value, s)
                    | _ -> effect.UnPack {
                        new Lambda with
                            member self.Invoke<'X> (k' : 'X -> Effect) = 
                                fun x -> loop s k (k' x)
                    }
            let (Eff effK) = eff
            let effect = effK done'
            Eff (fun k -> loop [] k effect)