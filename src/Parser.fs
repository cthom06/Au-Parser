namespace Cthom06.Au

type Parser<'char, 'a> =
    | Done of 'a
    | End of 'a
    | Pick of ('char -> Parser<'char, 'a> option)
    | Or of Parser<'char, 'a> Lazy * Parser<'char, 'a> Lazy

module Parser =
    type Error<'char,'a> = | Success of 'a | Error of (Parser<'char,'a> * 'char list)

    let rec bind p f =
        match p with
        | Done a
        | End a -> f a
        | Pick g -> Pick (g >> Option.map (fun ng -> bind ng f))
        | Or (g, h) -> Or (lazy (bind g.Value f), lazy (bind h.Value f))

    let rec map p f =
        match p with
        | Done a -> Done (f a)
        | End a -> End (f a)
        | Pick g -> Pick (g >> Option.map (fun ng -> map ng f))
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
        | Pick cf, x::xs ->
            match cf x with
            | Some v -> eval v xs
            | None -> Error (p, l), l
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
        
    let filter cf f = Pick (fun c -> if cf c then Some (f c) else None)

    let exactly c v = filter ((=) c) (fun _ -> v)

    let sequence s v =
        bind (Seq.fold (fun d c -> bind d  (fun _ -> exactly c (Done ()))) (Done ()) s) (fun _ -> Done v)
        
    let repeat p =
        let rec inner d =
            Or ( lazy(bind p (fun v -> inner (v::d))),
                 lazy(Done (List.rev d)))
        inner []
        
    let rec eat a b =
        Or ( lazy (filter a (fun _ -> eat a b)),
             lazy (b) )
        
    let eatWs b = eat System.Char.IsWhiteSpace b

    let range r f = filter (fun c -> List.exists ((=) c) r) f
    let except r f = filter (fun c -> not <| List.exists ((=) c) r) f
        
module Operators =
    let (>>=) a b = Parser.bind a b
    let ( *> ) a b = a >>= fun _ -> b
    let (<*>) a b = Parser.map a b // <$>
    let (<|>) a b = Or (a, b)
    let (<~>) a b = Or (lazy (a), lazy (b))
