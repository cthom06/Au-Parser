namespace Cthom06.Au

type Parser<'char, 'a> =
    | Done of 'a
    | Delay of (unit -> Parser<'char, 'a>)
    | Pick of ('char option -> Parser<'char, 'a> option)
    | Or of Parser<'char, 'a> * Parser<'char, 'a>

module Parser =
    type Error<'char,'a> = | Success of 'a | Error of (Parser<'char,'a> * 'char list)

    let rec bind p f =
        match p with
        | Done a -> f a
        | Delay ua -> Delay (fun () -> bind (ua ()) f)
        | Pick g -> Pick (g >> Option.map (fun ng -> bind ng f))
        | Or (g, h) -> Or (bind g f, bind h f)

    let rec map p f =
        match p with
        | Done a -> Done (f a)
        | Delay ua -> Delay (fun () -> map (ua ()) f)
        | Pick g -> Pick (g >> Option.map (fun ng -> map ng f))
        | Or (g, h) -> Or (map g f, map h f)

    let eval p l =
        let rec evalStack p l stack =
            let inline fail stack p xs l =
                match stack with
                    | [] -> Error (p, xs), l
                    | (cl,cont)::nfails -> evalStack cont cl nfails
            match p, l with
            | Done v, xs -> Success v, xs
            | Delay up, xs -> evalStack (up ()) xs stack
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

    let exactly c = filter ((=) c) Done

    let sequence s = Seq.fold (fun d c -> bind d (fun _ -> map (exactly c) ignore)) (Done ()) s

    let repeat p =
        let rec inner d =
            Or ( bind p (fun v -> inner (v::d)),
                 Delay (fun () -> Done (List.rev d)))
        inner []

    let rec eatWhite =
        Or (Pick (fun mc ->
                match mc with
                | Some c when System.Char.IsWhiteSpace c -> Some eatWhite
                | _ -> None),
            Done ())

    let range r = filter (fun c -> List.exists ((=) c) r) Done
    let except r = filter (fun c -> not <| List.exists ((=) c) r) Done

    let endData = Pick (fun c -> match c with | None -> Some (Done ()) | _ -> None)

    let through a b = bind a (fun v -> bind b (fun _ -> Done v))

module Operators =
    let (>>=) a b = Parser.bind a b
    let ( *> ) a b = a >>= fun _ -> b
    let (<*>) a b = Parser.map a b // <$>
    let (<|>) a b = Or (a, b)
