module List' = struct
  include BatList
  include BatList.Exceptionless
end

module Result = BatResult

let (%) = Batteries.(%)

type coord = (int * int)

module Piece = struct
  type kind =
    | Amazon
    | Arrow
  type color =
    | Black
    | White
  type t = { color : color
           ; kind  : kind}

  let is_color : color -> t -> bool
    = fun color' {color} -> color = color'

  let is_white : t -> bool
    = function | {color = White} -> true
               | _             -> false

  let is_black : t -> bool
    = function | {color = Black} -> true
               | _               -> false

  let is_amazon : t -> bool
    = function | {kind = Amazon} -> true
               | _               -> false

  let is_arrow : t -> bool
    = function | {kind = Arrow} -> true
               | _              -> false

  let black : kind -> t
    = fun kind -> {color = Black; kind}

  let white : kind -> t
    = fun kind -> {color = White; kind}

  let make : color -> kind -> t
    = fun color kind -> {color; kind}

end

module Square = struct
  type t = { coord : coord
           ; piece : Piece.t option}

  let make : coord -> Piece.t -> t
    = fun coord piece -> {coord; piece = Some piece}

  let empty : coord -> t
    = fun coord -> {coord; piece = None}

  let is_empty : t -> bool
    = function | {piece = None} -> true
               | _              -> false

  let piece : t -> Piece.t option
    = fun {piece} -> piece

  let coord : t -> coord
    = fun {coord} -> coord

end

module Board = struct
  module Sq = Square
  module Pc = Piece

  type coord = int * int
  type t = Sq.t list

  (* TODO Rename to "new" *)
  let empty : t =
    let coord_values = (Aux.range 0 9) in
    let coords =
      List.map (fun x -> List.map (fun y -> (x, y)) coord_values) coord_values
      |> List.concat
    in
    List.map Sq.empty coords

  let square
    : t -> coord -> Sq.t
    = fun board coord' ->
      let square_with_coord Sq.{coord} = (coord = coord') in
      List.find square_with_coord board

  let select_square
    : coord -> t -> (Sq.t * t)
    = fun coord board ->
      let square = square board coord  in
      (square, List'.remove board square)

  (** [place coord piece board] is the [Result.Ok board'] of placing [piece] on
      the empty position on [board] designated by [coord], or the [Result.Bad
      piece'] of the [piece'] already occupying the position designated by
      [coord] on [board]. *)

  let place : coord -> Pc.t -> t -> (t, Sq.t) Result.t
    = fun coord piece board ->
      match select_square coord board with
      | Sq.{piece=None}, board' -> Result.Ok (Sq.make coord piece :: board')
      | square, _               -> Result.Bad square

  let remove : coord -> t -> (Pc.t * t, Sq.t) Result.t
    = fun coord board ->
      let (square, board') = select_square coord board in
      match square with
      | Sq.{piece=Some piece} ->
        if Piece.is_amazon piece
        then Result.Ok  (piece, board')
        else Result.Bad square
      | _ -> Result.Bad square

  let line_of_squares
    : coord -> coord -> t -> Sq.t list option
    = fun a b board ->
      let singleton ls = List.length ls = 1 in
      let same_length ls ms = List.length ls = List.length ms in
      let repeat_to_length a ls = Aux.repeat a (List.length ls)
      in
      let (x1, y1) = min a b
      and (x2, y2) = max a b
      in
      let xs = Aux.range x1 x2
      and ys = Aux.range y1 y2
      in
      let sequence_option =
        if      singleton xs      then Some (repeat_to_length x1 ys, ys)
        else if singleton ys      then Some (xs, repeat_to_length y1 xs)
        else if same_length xs ys then Some (xs, ys)
        else None
      in
      let open BatOption.Infix in
      sequence_option
      >>= fun (xs, ys) -> Some (List.combine xs ys)
      >>= fun coords   -> Some (List.map (square board) coords)

  let path_between
    : coord -> coord -> t -> Sq.t list option
    = fun source target board ->
      let open BatOption.Infix in
      line_of_squares source target board
      >>= List'.tl
      >>= fun tail -> Some (BatList.take (List.length tail - 1) tail)

  let all_squares_are_empty : Sq.t list -> bool
    = List.for_all Sq.is_empty

  (** [only_empty_squares squares] will [Return.Ok squares] if all the squares
      are empty or else [Return.Bad square] where square is the first non-empty
      square encountered *)
  let only_empty_squares : Sq.t list -> (Sq.t list, Sq.t) Result.t
    = fun squares -> match List'.find (Sq.is_empty) squares with
      | Some square -> Result.Bad square
      | None        -> Result.Ok squares

  let path_from_valid_piece
    : Pc.color -> coord -> coord -> t -> (Sq.t list, Sq.t) Result.t
    = fun color source target board ->
      let source_sq = square board source in
      let open BatOption.Infix in
      let valid_path =
        Sq.piece source_sq                                (* Source Square is non-empty *)
        >>= Aux.option_of_condition (Pc.is_color color)   (* is the appropriate color *)
        >>= Aux.option_of_condition Pc.is_amazon          (* and has an amazon *)
        >>= fun piece -> path_between source target board (* A valid path exists *)
      in
      match valid_path with
      | Some path -> Result.Ok path
      | None      -> Result.Bad source_sq

  let clear_path_from_valid_piece
    : Pc.color -> coord -> coord -> t -> (Sq.t list, Sq.t) Result.t
    = fun color source target board ->
      let open Result.Infix in
      path_from_valid_piece color source target board
      >>= only_empty_squares

  type change = Pc.color -> coord -> coord -> t -> (t, Sq.t) Result.t

  let fire : Pc.color -> coord -> coord -> t -> (t, Sq.t) Result.t
    = fun color source target board ->
      let open Result.Infix in
      clear_path_from_valid_piece color source target board
      >>= fun _ -> place target Pc.(make color Arrow) board

  let move : Pc.color -> coord -> coord -> t -> (t, Sq.t) Result.t
    = fun color source target board ->
      let open Result.Infix in
      clear_path_from_valid_piece color source target board
      >>= fun _                -> remove source board
      >>= fun (amazon, board') -> place target amazon board'
end

module Turn = struct

  type t = { color:Piece.color
           ; board:Board.t }

  let next : t -> Board.t -> t
    =
    let toggle = function
      | Piece.White -> Piece.Black
      | Piece.Black -> Piece.White
    in
    fun turn board -> {color = toggle turn.color; board}

  open Result.Infix

  let move
    : t -> coord -> coord -> (t, Square.t) Result.t
    = fun turn source target ->
      Board.move turn.color source target turn.board
      >>= fun board -> Result.Ok {turn with board}

  let fire
    : t -> coord -> coord -> (t, Square.t) Result.t
    = fun turn source target ->
      Board.fire turn.color source target turn.board
      >>= fun board -> Result.Ok (next turn board)
end

type t = Turn.t list
