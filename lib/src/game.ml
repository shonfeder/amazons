open Core

type coord = (int * int)
[@@deriving show(* , yojson *)]

exception Read_coord
let read_coord
  : string -> coord
  = fun str ->
  let parens_stripped = String.sub str 1 (String.length str - 2) in
  let (a, b) =
    match String.split ~on:',' parens_stripped with
    | [a; b] -> (String.strip a, String.strip b)
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
  [@@deriving show]
  type color =
    | Black
    | White
  [@@deriving show]
  type t =
    { color : color
    ; kind  : kind }
  [@@deriving show]

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
  (** A square is a [{coord : coord; piece : Piece.t option}] where
      an empty square is designated by [piece = Nothing]. *)
  type t =
    { coord : coord
    ; piece : Piece.t option}
  [@@deriving show]

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

  type t =
    Sq.t list
  [@@deriving show]

  exception Board

  type illegal_move =
    | Blocked       of Sq.t
    | Empty         of Sq.t
    | Occupied      of Sq.t
    | Invalid_move  of Sq.t * Sq.t
    | Invalid_piece of Sq.t
    | Wrong_color   of Sq.t
  [@@deriving show]

  type bad_move =
    { board  : t
    ; reason : illegal_move }
  [@@deriving show]

  type result_of_move =
    (t, bad_move) result
  [@@deriving show]

  let empty : t =
    let coord_values = (List.range 0 10) in
    let coords =
      List.map coord_values
        ~f:(fun x ->
            List.map coord_values
              ~f:(fun y -> (x, y)))
      |> List.concat
    in
    List.map coords
      ~f:Sq.empty

  (** [square board coord] is the [square] on the [board] at [coord] *)
  let square
    : t -> coord -> Sq.t
    = fun board coord' ->
      let square_with_coord Sq.{coord} = (coord = coord') in
        List.find_exn ~f:square_with_coord board

  (** [select_square coord board] is the pair [sq, rest] where [sq] is the
      square on [board] at [coord] and [rest] are all the squares on the board
      except [sq].*)
  let select_square
    : coord -> t -> (Sq.t * t)
    = fun coord board ->
      let square = square board coord  in
      (square, List.filter ~f:(fun x -> not (x = square)) board)

  (** [place coord piece board] is the [Ok board'] of placing [piece] on
      the empty position on [board] designated by [coord], or the [Error
      (sq:Sq.t)] of the designed [sq:Sq.t] which is already occupied by a
      piece *)
  let place
    : coord -> Pc.t -> t -> result_of_move
    = fun coord piece board ->
      match select_square coord board with
      | Sq.{piece=None}, board' -> Ok (Sq.make coord piece :: board')
      | square, _               -> Error {reason = Occupied square; board}

  (** [remove coord board] is the [Result.Ok (pc:Pc.t, board')] of removing
      [piece] from the [sq:Sq.t] on the [board] designated by [coord], or the
      [Error {reason; board}] where [reason] is either an [Empty square] or an
      [Occupied square] by an (imovable) [Pc.Arrow] *)
  let remove
    : coord -> t -> (Pc.t * t, bad_move) result
    = fun coord board ->
      let (sq, board') = select_square coord board in
      match Sq.piece sq with
      | Some piece ->
        if Piece.is_amazon piece
        then Ok (piece, Sq.{sq with piece = None} :: board')
        else Error {reason = Invalid_piece sq; board}
      | None -> Error {reason = Empty sq; board}

  let setup : t =
    let empty_board = Result.Ok empty
    and starting_positions =
      [ Pc.Black, (6,9) ; Pc.Black, (9,6) ; Pc.Black, (6,0) ; Pc.Black, (9,3)
      ; Pc.White, (0,6) ; Pc.White, (3,9) ; Pc.White, (0,3) ; Pc.White, (3,0) ]
    in
    let open Result.Monad_infix in
    let place boardM (color, coord) =
      boardM >>= fun board -> place coord Piece.(make color Amazon) board
    in
    match List.fold_left ~f:place ~init:empty_board starting_positions with
    | Ok board -> board       (* Return a set up board, *)
    | Error _  -> raise Board (* Should be impossible *)

  let line_of_squares
    : coord -> coord -> t -> Sq.t list option
    = fun a b board ->
      let is_singleton ls = List.length ls = 1 in
      let repeat_to_length a ls = List.init (List.length ls) ~f:(fun _ -> a)
      in
      let (x1, y1) = min a b
      and (x2, y2) = max a b
      in
      let xs = List.range x1 x2 ~stop:`inclusive
      and ys = List.range y1 y2 ~stop:`inclusive
      in
      let sequence_option =
        if is_singleton xs then Some (repeat_to_length x1 ys, ys) else
        if is_singleton ys then Some (xs, repeat_to_length y1 xs)
        else Some (xs, ys)
      in
      let open Option.Monad_infix in
      sequence_option
      >>= fun (xs, ys) -> List.zip xs ys
      >>= fun coords   -> Some (List.map ~f:(square board) coords)

  let path_between
    : coord -> coord -> t -> Sq.t list option
    = fun source target board ->
      let open Option.Monad_infix in
      line_of_squares source target board
      >>= List.tl >>| fun tail -> List.take tail (List.length tail - 1)

  let all_squares_are_empty : Sq.t list -> bool
    = List.for_all ~f:Sq.is_empty

  (** [only_empty_squares squares] is [Result.Ok squares] if all the squares are
      empty or else [Result.Error illegal_move] indicating the first in the list
      and the first non-empty square *)
  let only_empty_squares
    : Sq.t list -> (Sq.t list, bad_move) result
    = fun squares -> match List.find ~f:Sq.is_empty squares with
      | None ->
        Result.Ok squares
      | Some occupied_sq ->
        Result.Error { reason = Blocked occupied_sq
                     ; board  = squares }
  (* [squares] is not actually a board here, but it'll do... *)

  let path_from_valid_piece
    : Pc.color -> coord -> coord -> t -> (Sq.t list, bad_move) result
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
    : Pc.color -> coord -> coord -> t -> (Sq.t list, bad_move) result
    = fun color source target board ->
      let open Result.Monad_infix in
      path_from_valid_piece color source target board
      >>= only_empty_squares

  let fire
    : Pc.color -> coord -> coord -> t -> result_of_move
    = fun color source target board ->
      let open Result.Monad_infix in
      clear_path_from_valid_piece color source target board
      >>= fun _ -> place target Pc.(make color Arrow) board

  let move
    : Pc.color -> coord -> coord -> t -> result_of_move
    = fun color source target board ->
      let open Result.Monad_infix in
      clear_path_from_valid_piece color source target board
      >>= fun _                -> remove source board
      >>= fun (amazon, board') -> place target amazon board'
end

module Turn = struct

  type t =
    { color : Piece.color
    ; board : Board.t }
  [@@deriving show]

  (* let first : t =
   *   { color = Piece.White
   *   ; board = Board.setup }
   * [@@deriving yojson] *)

  let switch
    : Piece.color -> Piece.color = function
    | Piece.White -> Piece.Black
    | Piece.Black -> Piece.White

  let next
    : t -> Board.t -> t
    = fun field board ->
      { color = switch field.color
      ; board }

  open Result.Monad_infix

  type action = t -> coord -> coord -> (t, Board.bad_move) result

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
[@@deriving show]

module Update = struct

  exception Update_invalid

  type state = { game   : t
               ; source : coord
               ; target : coord }

  type msg =
    | Start
    | Fire of state
    | Move of state

  (* let start : t = [Turn.first] *)

  (* let send
   *   : msg -> (t, Board.bad_move) result =
   *   let take action {game; source; target} =
   *     match game with
   *     | [] -> raise Update_invalid
   *     | (turn :: _) as turns ->
   *       let open Result.Monad_infix in
   *       action turn source target
   *       >>= fun turn' -> Result.Ok (turn' :: turns)
   *   in
   *   function
   *   | Start      -> Result.Ok start
   *   | Move state -> take Turn.move state
   *   | Fire state -> take Turn.fire state *)

end
