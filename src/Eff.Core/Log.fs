namespace Eff.Core


// Basic log operation
type Log<'S, 'Eff when 'Eff :> Effect>(v : 'S, k : unit -> 'Eff) =
    interface Effect
    member self.Value = v
    member self.K = k


module Log = 

    // log helper functions
    let log (v : 'S) : Eff<unit, Effect> =
        shift (fun k -> new Log<'S, Effect>(v, k) :> _)


    // log effect handlers
    let pureLogHandler<'T, 'S> (eff : Eff<'T, Effect>) : Eff<'T * list<'S>, Effect> =
        let rec loop (s : list<'S>) (resultK : 'T * list<'S> -> Effect) (effK : Effect -> (Effect -> EffCont<Effect> -> Effect) -> Effect) (effect : Effect) (k : Effect -> EffCont<Effect> -> Effect) : Effect = 
            match effect with
            | :? Log<'S, Effect> as log -> loop (log.Value :: s)  resultK effK (log.K ()) k
            | :? Done<'T> as done' -> 
                let effect' = resultK (done'.Value, s)
                effK effect' (fun effect' (EffCont effK') -> k effect' (EffCont (loop s resultK effK')))
            | _ ->
                effK effect (fun effect' (EffCont effK') -> k effect' (EffCont (loop s resultK effK')))
        Eff (fun (k, exK, EffCont effK) -> 
                    let (Eff cont) = eff 
                    let effK' = loop [] k effK
                    let effect = cont (done', exK, EffCont effK')
                    runEffCont effect (EffCont effK')) 

    let consoleLogHandler<'T, 'S> (eff : Eff<'T, Effect>) : Eff<'T, Effect> =
        let rec loop (resultK : 'T -> Effect) (effK : Effect -> (Effect -> EffCont<Effect> -> Effect) -> Effect) (effect : Effect) (k : Effect -> EffCont<Effect> -> Effect) : Effect = 
            match effect with
            | :? Log<'S, Effect> as log -> printfn "Log: %A" log.Value; loop resultK effK (log.K ()) k
            | :? Done<'T> as done' -> 
                let effect' = resultK done'.Value
                effK effect' (fun effect' (EffCont effK') -> k effect' (EffCont (loop resultK effK')))
            | _ ->
                effK effect (fun effect' (EffCont effK') -> k effect' (EffCont (loop resultK effK')))
        Eff (fun (k, exK, EffCont effK) -> 
                    let (Eff cont) = eff 
                    let effK' = loop k effK
                    let effect = cont (done', exK, EffCont effK')
                    runEffCont effect (EffCont effK')) 

       