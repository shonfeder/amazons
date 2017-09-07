let assoc_select : 'a -> ('a * 'b) list -> 'b * ('a * 'b) list
  = fun a assoc ->
    let b = List.assoc a assoc in
    let assoc' = List.remove_assoc a assoc in
    (b, assoc')

let rec range : int -> int -> int list
  = fun n m -> if n > m then [] else n :: range (n + 1) m

let rec repeat : 'a -> int -> 'a list
  = fun a -> function
    | 0 -> []
    | n -> a :: repeat a (n - 1)

let rec last : 'a list -> 'a
  = function | [x]   -> x
             | _::xs -> last xs
             | _     -> raise (Failure "List has no last")

let option_of_condition : ('a -> bool) -> 'a -> 'a option
  = fun p x -> if p x then Some x else None

let condition_on_option : ('a -> bool) -> 'a option -> bool
  = fun p -> function | None   -> false
                      | Some x -> p x
