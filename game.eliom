[%%shared.start]

open Aux.Infix

module Result = CCResult

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

  type coord =
    int * int
  [@@deriving json]

  type t =
    Sq.t list
  [@@deriving json]

  type illegal_move =
    | Blocked       of Sq.t
    | Empty         of Sq.t
    | Occupied      of Sq.t
    | Invalid_move  of Sq.t * Sq.t
    | Invalid_piece of Sq.t
    | Wrong_color   of Sq.t
  [@@deriving json]

  type bad_move =
    { board  : t
    ; reason : illegal_move }
  [@@deriving json]

  type result_of_move =
    (t, bad_move) Result.t
  (* [@@deriving json] *)

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
      (square, CCList.remove square board)

  (** [place coord piece board] is the [Result.Ok board'] of placing [piece] on
      the empty position on [board] designated by [coord], or the [Result.Error
      (sq:Sq.t)] of the designed [sq:Sq.t] which is already occupied by a
      piece *)

  let place
    : coord -> Pc.t -> t -> result_of_move
    = fun coord piece board ->
      match select_square coord board with
      | Sq.{piece=None}, board' -> Result.Ok (Sq.make coord piece :: board')
      | square, _               -> Result.Error {reason = Occupied square; board}

  (** [remove coord board] is the [Result.Ok (pc:Pc.t, board')] of removing
      [piece] from the [sq:Sq.t] on the [board] designated by [coord], or the
      [Result.Error (sq:Sq.t)] of the designated [sq:Sq.t] which is empty or
      ocuppied by an (imovable) [Pc.Arrow] *)

  let remove
    : coord -> t -> (Pc.t * t, bad_move) Result.t
    = fun coord board ->
      let (square, board') = select_square coord board in
      match Sq.piece square with
      | Some piece ->
        if Piece.is_amazon piece
        then Result.Ok (piece, board')
        else Result.Error {reason = Invalid_piece square; board}
      | None -> Result.Error {reason = Empty square; board}

  let setup : t =
    let open CCResult.Infix in
    let empty_board = Result.Ok empty
    and starting_positions =
      [ Pc.Black, (6,9) ; Pc.Black, (9,6) ; Pc.Black, (6,0) ; Pc.Black, (9,3)
      ; Pc.White, (0,6) ; Pc.White, (3,9) ; Pc.White, (0,3) ; Pc.White, (3,0) ]
    in
    let place boardM (color, coord) =
      boardM >>= fun board -> place coord Piece.(make color Amazon) board
    in
    List.fold_left place empty_board starting_positions
    |> CCResult.get_or ~default:empty (* Return a set up board or an empty board*)
  (* An empty board should be possible. ?? *)

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
      let open CCOpt.Infix in
      sequence_option
      >>= fun (xs, ys) -> Some (List.combine xs ys)
      >>= fun coords   -> Some (List.map (square board) coords)

  let path_between
    : coord -> coord -> t -> Sq.t list option
    = fun source target board ->
      let open CCOpt.Infix in
      line_of_squares source target board
      >>= fun sqs -> if CCList.is_empty sqs then None else Some (List.tl sqs)
      >>= fun tail -> Some (CCList.take (List.length tail - 1) tail)

  let all_squares_are_empty : Sq.t list -> bool
    = List.for_all Sq.is_empty

  (** [only_empty_squares squares] is [Result.Ok squares] if all the squares are
      empty or else [Result.Error illegal_move] indicating the first in the list
      and the first non-empty square *)
  let only_empty_squares
    : Sq.t list -> (Sq.t list, bad_move) Result.t
    = fun squares -> match CCList.find_pred Sq.is_empty squares with
      | None ->
        Result.Ok squares
      | Some occupied_sq ->
        Result.Error { reason = Blocked occupied_sq
                     ; board  = squares }
  (* [squares] is not actually a board here, but it'll do... *)

  let path_from_valid_piece
    : Pc.color -> coord -> coord -> t -> (Sq.t list, bad_move) Result.t
    = fun color source_coord target_coord board ->
      let source   = square board source_coord
      and target   = square board target_coord
      and path_opt = path_between source_coord target_coord board
      in
      let valid_path =
        match Sq.piece source with
        | None -> Result.Error (Empty source)
        | Some pc ->
          if
            not (Pc.is_color color pc) then Result.Error (Wrong_color   source)
          else if
            not (Pc.is_amazon pc)      then Result.Error (Invalid_piece source)
          else
            match path_opt with
            | None      -> Result.Error (Invalid_move (source, target))
            | Some path -> Result.Ok path
      in
      match valid_path with
      | Result.Error reason -> Result.Error {board; reason}
      | Result.Ok path      -> Result.Ok path

  let clear_path_from_valid_piece
    : Pc.color -> coord -> coord -> t -> (Sq.t list, bad_move) Result.t
    = fun color source target board ->
      let open Result.Infix in
      path_from_valid_piece color source target board
      >>= only_empty_squares

  let fire
    : Pc.color -> coord -> coord -> t -> result_of_move
    = fun color source target board ->
      let open Result.Infix in
      clear_path_from_valid_piece color source target board
      >>= fun _ -> place target Pc.(make color Arrow) board

  let move
    : Pc.color -> coord -> coord -> t -> result_of_move
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

  type action = t -> coord -> coord -> (t, Board.bad_move) Result.t

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
[@@deriving json]

module Update = struct

  exception Update_invalid

  type state = { game   : t
               ; source : coord
               ; target : coord }

  type msg =
    | Start
    | Fire of state
    | Move of state

  type result = (t, Board.bad_move) Result.t

  let start : t = [Turn.first]

  let send
    : msg -> result =
    let take action {game; source; target} =
      match game with
      | [] -> raise Update_invalid
      | (turn :: _) as turns ->
        let open Result.Infix in
        action turn source target
        >>= fun turn' -> Result.Ok (turn' :: turns)
    in
    function
    | Start      -> Result.Ok start
    | Move state -> take Turn.move state
    | Fire state -> take Turn.fire state

end
