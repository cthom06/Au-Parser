namespace Cthom06.Au

type Parser<'a> =
    | Done of 'a
    | End of 'a
    | Except of char list * (char -> 'a Parser)
    | Range of char list * (char -> 'a Parser)
    | Or of 'a Parser Lazy * 'a Parser Lazy
    with
        override this.ToString () =
            match this with
            | Done _ -> "[Completed Computation]"
            | End _ -> "[End of Input]"
            | Except (cs, _) -> "[Any except " + System.String.Join(" ", (cs |> Seq.map string)) + "]"
            | Range (cs, _) -> "[Any of " + System.String.Join(" ", (cs |> Seq.map string)) + "]"
            | Or (p1, p2) -> p1.ToString () + " or " + p2.ToString ()

module Parser =
    type Error<'a> = | Success of 'a | Error of ('a Parser * char list)

    let rec bind p f =
        match p with
        | Done a
        | End a -> f a
        | Except (cs, g) -> Except (cs, fun c -> bind (g c) f)
        | Range (cs, g) -> Range (cs, fun c -> bind (g c) f)
        | Or (g, h) -> Or (lazy (bind g.Value f), lazy (bind h.Value f))
        
    let rec map p f =
        match p with
        | Done a -> Done (f a)
        | End a -> End (f a)
        | Except (cs, g) -> Except (cs, fun c -> map (g c) f)
        | Range (cs, g) -> Range (cs, fun c -> map (g c) f)
        | Or (g, h) -> Or (lazy (map g.Value f), lazy (map h.Value f))

    let rec orOf steps =
        match steps with
        | a::b::[] -> Or (a, b)
        | x::xs -> Or (x, lazy(orOf xs))
        | [] -> raise (new System.InvalidOperationException ())

    let rec eval p l =
        match p, l with
        | Done v, xs -> Success v, xs
        | End v, [] -> Success v, []
        | Range (cs, f), x::xs when List.contains x cs ->
            eval (f x) xs
        | Except (cs, f), x::xs when not (List.contains x cs) ->
            eval (f x) xs
        | Or (p1, p2), l ->
            match eval p1.Value l with
            | Error (e1,r1), _ ->
                match eval p2.Value l with
                | Error (e2, r2), _ ->
                    if List.length r2 < List.length r1 then
                        Error (e2,r2), l
                    else
                        Error (e1, r1), l
                | Success v, xs -> Success v, xs
            | Success v, xs -> Success v, xs
        | p, xs -> Error (p, xs), l
        
    let char c v = Range ([c], (fun _ -> v))

    let word s v =
        bind (Seq.fold (fun d c -> bind d  (fun _ -> char c (Done ()))) (Done ()) s) (fun _ -> Done v)
        
    let repeat p =
        let rec inner d =
            Or ( lazy(bind p (fun v -> inner (v::d))),
                 lazy(Done (List.rev d)))
        inner []
        
    let rec eat a b =
        Or ( lazy (Range (a, fun _ -> eat a b)),
             lazy (b) )
        
    let eatWs b = eat [' '; '\r'; '\n'; '\t'] b
        
    module Operators =
        let (>>=) a b = bind a b
        let ( *> ) a b = a >>= fun _ -> b
        let (<*>) a b = map a b // <$>
        let (<|>) a b = Or (a, b)
        let (<~>) a b = Or (lazy (a), lazy (b))
