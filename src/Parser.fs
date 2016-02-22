namespace Cthom06.Au

type Parser<'char, 'a> =
    | Done of 'a
    | Pick of ('char option -> Parser<'char, 'a> option)
    | Or of Parser<'char, 'a> * Parser<'char, 'a> Lazy

module Parser =
    type Error<'char,'a> = | Success of 'a | Error of (Parser<'char,'a> * 'char list)

    let rec bind p f =
        match p with
        | Done a -> f a
        | Pick g -> Pick (g >> Option.map (fun ng -> bind ng f))
        | Or (g, h) -> Or (bind g f, lazy (bind h.Value f))

    let rec map p f =
        match p with
        | Done a -> Done (f a)
        | Pick g -> Pick (g >> Option.map (fun ng -> map ng f))
        | Or (g, h) -> Or (map g f, lazy (map h.Value f))

    let eval p l =
        let rec evalStack p l stack =
            let inline fail (stack : (_ * _ Lazy) list) p xs l =
                match stack with
                    | [] -> Error (p, xs), l
                    | (cl,cont)::nfails -> evalStack cont.Value cl nfails
            match p, l with
            | Done v, xs -> Success v, xs
            | Pick cf, [] ->
                match cf None with
                | Some v -> evalStack v [] stack
                | None -> fail stack p l l
            | Pick cf, x::xs ->
                match cf (Some x) with
                | Some v -> evalStack v xs stack
                | None -> fail stack p l l
            | Or (p1, p2), l ->
                evalStack p1 l ((l,p2)::stack)
        evalStack p l []

    let filter cf f = Pick (Option.bind (fun c -> if cf c then Some (f c) else None))

    let exactly c v = filter ((=) c) (fun _ -> v)

    let sequence s v =
        bind (Seq.fold (fun d c -> bind d (fun _ -> exactly c (Done ()))) (Done ()) s) (fun _ -> Done v)

    let repeat p =
        let rec inner d =
            Or ( bind p (fun v -> inner (v::d)),
                 lazy (Done (List.rev d)))
        inner []

    let rec eatWhite =
        Or (Pick (fun mc ->
                match mc with
                | Some c when System.Char.IsWhiteSpace c -> Some eatWhite
                | _ -> None),
            lazy (Done ()))

    let range r f = filter (fun c -> List.exists ((=) c) r) f
    let except r f = filter (fun c -> not <| List.exists ((=) c) r) f

    let endData v = Pick (fun c -> match c with | None -> Some (Done v) | _ -> None)

module Operators =
    let (>>=) a b = Parser.bind a b
    let ( *> ) a b = a >>= fun _ -> b
    let (<*>) a b = Parser.map a b // <$>
    let (<~>) a b = Or (a, b)
    let (<|>) a b = Or (a, Lazy.CreateFromValue b)
