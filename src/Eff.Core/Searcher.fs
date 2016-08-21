namespace Eff.Core

// basic operation
type Search<'S, 'Eff when 'Eff :> Effect>(v : 'S, k : bool -> 'Eff) =
    interface Effect
    member self.Value = v
    member self.K = k


module Searcher = 

    // helper functions
    let search (v : 'S) : Eff<bool, Effect> =
        shift (fun k -> new Search<'S, Effect>(v, k) :> _)

    // effect handlers
    let findNeighborhoodHandler (eff : Eff<bool, Effect>) : Eff<bool * list<int * bool>, Effect> =
        let rec loop (s : list<int * bool>) (resultK : bool * list<int * bool> -> Effect) (effK : Effect -> (Effect -> EffCont<Effect> -> Effect) -> Effect) (effect : Effect) (k : Effect -> EffCont<Effect> -> Effect) : Effect = 
            match effect with
            | :? Search<int, Effect> as search -> 
                let result = s |> List.tryFind (fun (n, b) -> n = search.Value)
                match result with
                | Some (n, b) -> loop s resultK effK (search.K b) k
                | None -> loop ((search.Value, true) :: s) (function (true, s') -> resultK (true, s') 
                                                                   | (false, _) -> loop ((search.Value, false) :: s) resultK effK (search.K false) k) 
                               effK (search.K true) k
            | :? Done<bool> as done' -> 
                let effect' = resultK (done'.Value, s)
                effK effect' (fun effect' (EffCont effK') -> k effect' (EffCont (loop s resultK effK')))
            | _ ->
                effK effect (fun effect' (EffCont effK') -> k effect' (EffCont (loop s resultK effK')))
        Eff (fun (k, exK, EffCont effK) -> 
                    let (Eff cont) = eff 
                    let effK' = loop [] k effK
                    let effect = cont (done', exK, EffCont effK')
                    runEffCont effect (EffCont effK')) 