[%%shared.start]

module List' = struct
  include BatList
  include BatList.Exceptionless
end

module Result = BatResult

let (%) = Batteries.(%)

type coord = (int * int)
[@@deriving show, json]

exception Read_coord
let read_coord
  : string -> coord
  = fun str ->
  let parens_stripped = String.sub str 1 (String.length str - 2) in
  let (a, b) =
    match String.split_on_char ',' parens_stripped with
    | [a; b] -> (a, b)
    | _      -> raise Read_coord
  in
  try
    (int_of_string a, int_of_string b)
  with
  | Failure _ -> raise Read_coord

module Piece = struct
  type kind =
    | Amazon
    | Arrow
  [@@deriving json]
  type color =
    | Black
    | White
  [@@deriving json]
  type t =
    { color : color
    ; kind  : kind}
  [@@deriving json]

  let is_color : color -> t -> bool
    = fun color' {color} -> color = color'

  let is_white : t -> bool
    = function | {color = White} -> true
               | _               -> false

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
  type t =
    { coord : coord
    ; piece : Piece.t option}
  [@@deriving json]

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

  type coord = int * int [@@deriving json]
  type t = Sq.t list [@@deriving json]

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

  let place
    : coord -> Pc.t -> t -> (t, Sq.t) Result.t
    = fun coord piece board ->
      match select_square coord board with
      | Sq.{piece=None}, board' -> Result.Ok (Sq.make coord piece :: board')
      | square, _               -> Result.Bad square

  let remove
    : coord -> t -> (Pc.t * t, Sq.t) Result.t
    = fun coord board ->
      let (square, board') = select_square coord board in
      match square with
      | Sq.{piece=Some piece} ->
        if Piece.is_amazon piece
        then Result.Ok  (piece, board')
        else Result.Bad square
      | _ -> Result.Bad square

  let setup
    : t =
    let open BatResult.Infix in
    let empty_board = Result.Ok empty
    and starting_positions =
      [ Pc.Black, (6,9) ; Pc.Black, (9,6) ; Pc.Black, (6,0) ; Pc.Black, (9,3)
      ; Pc.White, (0,6) ; Pc.White, (3,9) ; Pc.White, (0,3) ; Pc.White, (3,0) ]
    in
    let place boardM (color, coord) =
      boardM >>= fun board -> place coord Piece.(make color Amazon) board
    in
    List.fold_left place empty_board starting_positions
    |> BatResult.default empty (* Return a setup board or an empty board*)
  (* An empty board should be possible. *)

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
        Sq.piece source_sq                                  (* Source Square is non-empty, *)
        >>= Aux.option_of_condition (Pc.is_color color)     (* is the appropriate color, *)
        >>= Aux.option_of_condition Pc.is_amazon            (* and has an amazon, *)
        >>= fun _amazon -> path_between source target board (* also a valid path exists. *)
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

  type t =
    { color : Piece.color
    ; board : Board.t }
  [@@deriving json]

  let first : t =
    { color = Piece.White
    ; board = Board.setup }
  [@@deriving json]

  let switch
    : Piece.color -> Piece.color = function
    | Piece.White -> Piece.Black
    | Piece.Black -> Piece.White

  let next
    : t -> Board.t -> t
    = fun field board ->
      { color = switch field.color
      ; board }

  open Result.Infix

  type action = t -> coord -> coord -> (t, Square.t) Result.t

  let move : action
    = fun turn source target ->
      Board.move turn.color source target turn.board
      >>= fun board -> Result.Ok {turn with board}

  let fire : action
    = fun turn source target ->
      Board.fire turn.color source target turn.board
      >>= fun board -> Result.Ok (next turn board)
end

type t = Turn.t list

let start : t = [Turn.first]
