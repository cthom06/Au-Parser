namespace Cthom06.Au

type Parser<'char, 'a> =
    | Done of 'a
    | End of 'a
    | Pick of ('char -> Parser<'char, 'a> option)
    | Or of Parser<'char, 'a> * Parser<'char, 'a> Lazy

module Parser =
    type Error<'char,'a> = | Success of 'a | Error of (Parser<'char,'a> * 'char list)

    let rec bind p f =
        match p with
        | Done a
        | End a -> f a
        | Pick g -> Pick (g >> Option.map (fun ng -> bind ng f))
        | Or (g, h) -> Or (bind g f, lazy (bind h.Value f))

    let rec map p f =
        match p with
        | Done a -> Done (f a)
        | End a -> End (f a)
        | Pick g -> Pick (g >> Option.map (fun ng -> map ng f))
        | Or (g, h) -> Or (map g f, lazy (map h.Value f))

    let rec orOf (steps : _ Lazy list) =
        match steps with
        | a::b::[] -> Or (a.Value, b)
        | x::xs -> Or (x.Value, lazy(orOf xs))
        | [] -> raise (new System.InvalidOperationException ())

    let eval p l =
        let rec evalStack p l stack =
            let inline fail (stack : _ Lazy list) p xs l =
                match stack with
                    | [] -> Error (p, xs), l
                    | cont::nfails -> evalStack cont.Value l nfails
            match p, l with
            | Done v, xs -> Success v, xs
            | End v, [] -> Success v, []
            | Pick cf, x::xs ->
                match cf x with
                | Some v -> evalStack v xs stack
                | None -> fail stack p l l
            | Or (p1, p2), l ->
                evalStack p1 l (p2::stack)
            | p, xs -> fail stack p xs l
        evalStack p l []
        
    let filter cf f = Pick (fun c -> if cf c then Some (f c) else None)

    let exactly c v = filter ((=) c) (fun _ -> v)

    let sequence s v =
        bind (Seq.fold (fun d c -> bind d  (fun _ -> exactly c (Done ()))) (Done ()) s) (fun _ -> Done v)
        
    let repeat p =
        let rec inner d =
            Or ( bind p (fun v -> inner (v::d)),
                 lazy(Done (List.rev d)))
        inner []
        
    let rec eat a b =
        Or ( filter a (fun _ -> eat a b),
             Lazy.CreateFromValue b )
        
    let eatWs b = eat System.Char.IsWhiteSpace b

    let range r f = filter (fun c -> List.exists ((=) c) r) f
    let except r f = filter (fun c -> not <| List.exists ((=) c) r) f
        
module Operators =
    let (>>=) a b = Parser.bind a b
    let ( *> ) a b = a >>= fun _ -> b
    let (<*>) a b = Parser.map a b // <$>
    let (<|>) a b = Or (a, b)
    let (<~>) a b = Or (a, Lazy.CreateFromValue b)
