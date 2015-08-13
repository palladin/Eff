# Eff
Eff is a library for programming with Algebraic Effects in F# inspired by the [Eff] programming language and the implementation of Algebraic Effects in [OCaml]. The main idea is to repurpose F#'s computation expressions exception handling mechanism for effect handling.

``` fsharp
// example
let test () = 
    eff {
        let! x = get ()
        do! put (x + 1)
        let! y = get ()
        do! put (y + y)
        return! get ()
    } 
    
let pureState<'T, 'Ans> (c : Eff<'T, int -> 'Ans>) : Eff<'T, int -> 'Ans> = 
    eff {
        try
            return! c
        with 
            | :? Get<int, int -> 'Ans> as get -> return! Eff (fun _ s -> get.K s s)
            | :? Put<int, int -> 'Ans> as put -> return! Eff (fun _ _ -> put.K () put.Value)
    }
    
// Apply state effect and execute
test () |> pureState |> run (fun x -> (fun s -> (x, s))) |> (fun f -> f 1) // // (4, 4)
```


[Eff]: http://math.andrej.com/wp-content/uploads/2012/03/eff.pdf
[OCaml]: http://www.lpw25.net/ocaml2015-abs2.pdf
