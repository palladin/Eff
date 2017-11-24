# Eff
Eff is a library for programming with Algebraic Effects in F# inspired by the [Eff] programming language and the implementation of Algebraic Effects in [OCaml]-Haskell([1], [2]) and especially from the paper [Eff Directly in OCaml].

``` fsharp
// state effect example
let test () = 
    eff {
        let! x = get ()
        do! put (x + 1)
        let! y = get ()
        do! put (y + y)
        return! get ()
    } 
    
// state effect handler
let stateHandler (s : 'S) (eff : Eff<#State<'S>, 'T>) : ('T * 'S) =
    let rec loop (s : 'S) (effect : Effect) = 
        match effect with
        | :? Get<'S> as get -> loop s (get.K s) 
        | :? Put<'S> as put -> loop put.Value (put.K ())
        | :? Done<'T> as done' -> (done'.Value, s)
        | _ -> failwith "Unhandled effect"
    loop s (run done' eff) 

// Apply state effect and execute
stateHandler 1 (test ()) // (4, 4)
```


[Eff]: http://math.andrej.com/wp-content/uploads/2012/03/eff.pdf
[OCaml]: http://www.lpw25.net/ocaml2015-abs2.pdf
[1]: http://homepages.inf.ed.ac.uk/slindley/papers/handlers.pdf
[2]: http://okmij.org/ftp/Haskell/extensible/more.pdf
[Eff Directly in OCaml]: http://kcsrk.info/papers/eff_ocaml_ml16.pdf
