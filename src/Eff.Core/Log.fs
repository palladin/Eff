namespace Eff.Core


// Basic log operation
type Log<'S> = inherit Effect

type LogEntry<'S>(v : 'S, k : unit -> Effect) =
    interface Log<'S> with
        member self.UnPack(lambda : Lambda) : Effect =
            new LogEntry<'S>(v, lambda.Invoke<unit> k) :> _
    member self.Value = v
    member self.K = k


module Log = 

    // log helper functions
    let log<'U, 'S when 'U :> Log<'S>> : 'S -> Eff<'U, unit> = 
        fun s -> shift (fun k -> new LogEntry<'S>(s, k) :> _)
    let logf fmt = Printf.ksprintf log fmt
    


    // log effect handlers
    let rec pureLogHandler<'U, 'S, 'A when 'U :> Log<'S>> 
        : Eff<'U, 'A> -> Eff<'U, 'A * list<'S>> = 
        fun eff ->
            let rec loop : list<'S> -> (('A * list<'S>) -> Effect) -> Effect -> Effect = 
                fun s k effect ->
                    match effect with
                    | :? LogEntry<'S> as log -> 
                        loop (log.Value :: s) k (log.K ()) 
                    | :? Done<'A> as done' -> k (done'.Value, s)
                    | _ -> 
                        effect.UnPack {
                            new Lambda with
                                member self.Invoke<'X> (k' : 'X -> Effect) = 
                                    fun x -> loop s k (k' x)
                        }
            let (Eff effK) = eff
            let effect = effK done'
            Eff (fun k -> loop [] k effect)

    let rec consoleLogHandler<'U, 'S, 'A when 'U :> Log<'S>> 
        : Eff<'U, 'A> -> Eff<'U, 'A> = 
        fun eff ->
            let rec loop : ('A -> Effect) -> Effect -> Effect = 
                fun k effect ->
                    match effect with
                    | :? LogEntry<'S> as log -> 
                        printfn "Log: %A" log.Value; loop k (log.K ())
                    | :? Done<'A> as done' -> k done'.Value
                    | _ -> effect.UnPack {
                        new Lambda with
                            member self.Invoke<'X> (k' : 'X -> Effect) = 
                                fun x -> loop k (k' x)
                    }
            let (Eff effK) = eff
            let effect = effK done'
            Eff (fun k -> loop k effect)


       