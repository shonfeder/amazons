open Core

let (%) f g = fun x -> f (g x)

module Coord = struct
  type t =
    { x : int
    ; y : int }
  [@@deriving show {with_path = false}, yojson]

  let min a b = let (x, y) = min (a.x, a.y) (b.x, b.y) in {x; y}
  let max a b = let (x, y) = max (a.x, a.y) (b.x, b.y) in {x; y}

  module Of = struct
    let pair (x, y) = {x; y}
  end
  module To = struct
    let pair {x; y} = (x, y)
  end
end

module Piece = struct
  type kind =
    | Amazon  [@printer fun fmt _ -> fprintf fmt "&"]
    | Arrow   [@printer fun fmt _ -> fprintf fmt "*"]
  [@@deriving show {with_path = false}, yojson]

  type color =
    | Black [@printer fun fmt _ -> fprintf fmt "B"]
    | White [@printer fun fmt _ -> fprintf fmt "W"]
  [@@deriving show {with_path = false}, yojson]

  type t =
    { color : color
    ; kind  : kind }
  [@@deriving yojson]

  (* E.g. '&W' *)
  let show
    : t -> string = function
    | {color; kind = Arrow} -> "**"
    | {color; kind} -> Printf.sprintf "%s%s" (show_kind kind) (show_color color)

  let pp
    : Format.formatter -> t -> unit
    = fun fmt pc ->
      Format.fprintf fmt "%s" (show pc)

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

  let color_of_string
    : string -> color option = function
    | "Black" -> Some Black
    | "White" -> Some White
    | _       -> None
end

module Square = struct
  (** A square is a [{coord : Coord.t; piece : Piece.t option}] where
      an empty square is designated by [piece = Nothing]. *)

  let show_piece : Piece.t option -> string = function
    | None                     -> "  "
    | Some Piece.{kind; color} ->
      if kind = Piece.Arrow
      then Piece.show_kind kind
      else (Piece.show_kind kind) ^ (Piece.show_color color)

  type t =
    { coord : Coord.t
    ; piece : Piece.t option }
  [@@deriving yojson]

  let show
    : t -> string = function
    | {piece = Some pc} -> Piece.show pc
    | {piece = None}    -> "  "

  let pp
    : Format.formatter -> t -> unit
    = fun fmt sq ->
      Format.fprintf fmt "%s" (show sq)

  let make : Coord.t -> Piece.t -> t
    = fun coord piece -> {coord; piece = Some piece}

  let empty : Coord.t -> t
    = fun coord -> {coord; piece = None}

  let is_empty : t -> bool
    = function | {piece = None} -> true
               | _              -> false

  let piece : t -> Piece.t option
    = fun {piece} -> piece

  let coord : t -> Coord.t
    = fun {coord} -> coord

  let piece_is
    : f:(Piece.t -> bool) -> t -> bool
    = fun ~f:f square -> Option.exists ~f square.piece
end

module Board = struct
  module Sq = Square
  module Pc = Piece

  type t =
    Sq.t list
  [@@deriving yojson]

  exception Board

  type illegal_move =
    | Blocked       of Sq.t
    | Empty         of Sq.t
    | Occupied      of Sq.t
    | Invalid_move  of Sq.t * Sq.t
    | Invalid_piece of Sq.t
    | Wrong_color   of Sq.t
  (* [@@deriving show, yojson] *)

  type bad_move =
    { board  : t
    ; reason : illegal_move }
  (* [@@deriving show, yojson] *)

  type result_of_move =
    (t, bad_move) result
  (* [@@deriving show] *)

  (** A list of all the coords for the squares on the board,
      starting from 0,0 *)
  let square_coords =
    let coord_values = (List.range 0 10) in
    List.map coord_values
      ~f:(fun x ->
          List.map coord_values
            ~f:(fun y -> Coord.{x; y}))
    |> List.concat

  let empty : t = List.map square_coords ~f:Sq.empty

  (** [square board coord] is the [square] on the [board] at [coord] *)
  let square
    : t -> Coord.t -> Sq.t
    = fun board coord' ->
      let square_with_coord Sq.{coord} = (coord = coord') in
        List.find_exn ~f:square_with_coord board

  (** [select_square coord board] is the pair [sq, rest] where [sq] is the
      square on [board] at [coord] and [rest] are all the squares on the board
      except [sq].*)
  let select_square
    : Coord.t -> t -> (Sq.t * t)
    = fun coord board ->
      let square = square board coord  in
      (square, List.filter ~f:(fun x -> not (x = square)) board)

  (** Break a list of squares into a list of lists of squares,
      each list containing the 10 squares of it's row *)
  let rows_of_squares
    : t -> Sq.t list list =
    fun board ->
      let row_coords = List.chunks_of ~length:10 square_coords in
      let append_row rows coords =
        let add_sq row coord = row @ [square board coord] in
        List.fold ~f:add_sq ~init:[] coords :: rows
      in
      List.fold ~f:append_row ~init:[] row_coords

  (** E.g.
     +--+--+--+ ... "horizontal_div"
     |&W|  |**| ...
     +--+--+--+ ...
     ..............
  *)
  let show
    : t -> string =
    fun board ->
      let horizontal_div =
        let div_char n = if n = 0 || 0 = n mod 3 then '+' else '-' in
        String.init 31 div_char
      and show_row row =
        let f acc sq = acc ^ Printf.sprintf "|%s" (Square.show sq) in
        (List.fold ~f ~init:"" row) ^ "|"
      in
      let rows = rows_of_squares board
      and f acc row =
        Printf.sprintf "%s%s\n%s\n" acc horizontal_div (show_row row)
      in
      (List.fold ~f ~init:"" rows) ^ horizontal_div

  let pp
    : Format.formatter -> t -> unit
    = fun fmt board ->
      Format.fprintf fmt "%s" (show board)

  (** [place coord piece board] is the [Ok board'] of placing [piece] on
      the empty position on [board] designated by [coord], or the [Error
      (sq:Sq.t)] of the designed [sq:Sq.t] which is already occupied by a
      piece *)
  let place
    : Coord.t -> Pc.t -> t -> result_of_move
    = fun coord piece board ->
      match select_square coord board with
      | Sq.{piece=None}, board' -> Ok (Sq.make coord piece :: board')
      | square, _               -> Error {reason = Occupied square; board}

  (** [remove coord board] is the [Result.Ok (pc:Pc.t, board')] of removing
      [piece] from the [sq:Sq.t] on the [board] designated by [coord], or the
      [Error {reason; board}] where [reason] is either an [Empty square] or an
      [Occupied square] by an (imovable) [Pc.Arrow] *)
  let remove
    : Coord.t -> t -> (Pc.t * t, bad_move) result
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
      let open Coord in
      [ Pc.Black, {x=0; y=6} ; Pc.Black, {x=3; y=9}
      ; Pc.Black, {x=6; y=9} ; Pc.Black, {x=9; y=6}
      ; Pc.White, {x=0; y=3} ; Pc.White, {x=3; y=0}
      ; Pc.White, {x=6; y=0} ; Pc.White, {x=9; y=3} ]
    in
    let open Result.Monad_infix in
    let place boardM (color, coord) =
      boardM >>= fun board -> place coord Piece.(make color Amazon) board
    in
    match List.fold_left ~f:place ~init:empty_board starting_positions with
    | Ok board -> board       (* Return a set up board, *)
    | Error _  -> raise Board (* Should be impossible *)

  let line_of_squares
    : Coord.t -> Coord.t -> t -> Sq.t list option
    = fun a b board ->
      let is_singleton ls = List.length ls = 1 in
      let repeat_to_length a ls = List.init (List.length ls) ~f:(fun _ -> a)
      in
      let min = Coord.min a b
      and max = Coord.max a b
      in
      let xs = List.range min.x max.x ~stop:`inclusive
      and ys = List.range min.y max.y ~stop:`inclusive
      in
      let sequence_option =
        if is_singleton xs then Some (repeat_to_length min.x ys, ys) else
        if is_singleton ys then Some (xs, repeat_to_length min.y xs)
        else Some (xs, ys)
      in
      let open Option.Monad_infix in
      sequence_option
      >>= fun (xs, ys) -> List.zip xs ys
      >>| List.map ~f:(square board % Coord.Of.pair)

  let path_between
    : Coord.t -> Coord.t -> t -> Sq.t list option
    = fun source target board ->
      let open Option.Monad_infix in
      line_of_squares source target board
      >>= List.tl >>| fun tail -> List.take tail (List.length tail - 1)

  let all_squares_are_empty : Sq.t list -> bool
    = List.for_all ~f:Sq.is_empty

  (** [only_empty_squares squares] is [Ok squares] if all the squares are empty
      or else [Error {reason; board}] where [reason] is [Blocked occupied_sq],
      indicating the first non-empty square. *)
  let only_empty_squares
    : Sq.t list -> (Sq.t list, bad_move) result
    = fun squares -> match List.find ~f:(not % Sq.is_empty) squares with
      | None             -> Ok squares
      | Some occupied_sq -> Error { reason = Blocked occupied_sq
                                  ; board  = squares }
  (* [squares] is not actually a board here, but it'll do... *)

  (** [square_with_amazon_of_color sq color] is [Ok (sq:Square.t)] if
      sq holds a [Piece.Amazon] of [color:Piece.color] or else

      - [Empty sq] if the [sq] is unoccupied
      - [Invalid_piece sq] if the [sq] does not contain a [Piece.Amazon]
      - [Wrong_color sq] if the piece on [sq] is not of [color] *)
  let square_with_amazon_of_color
    : Pc.color -> Sq.t -> (Sq.t, illegal_move) result
    = fun expected_color sq -> match sq with
      | Sq.{piece = None}                  -> Error (Empty sq)
      | Sq.{piece = Some Pc.{kind; color}} ->
        if kind  <> Amazon         then Error (Invalid_piece sq) else
        if color <> expected_color then Error (Wrong_color sq)
        else Ok sq

  let free_square
    : Sq.t -> (Sq.t, illegal_move) result
    = fun sq ->
      if Square.is_empty sq
      then Ok sq
      else Error (Occupied sq)

  let valid_path
    : Sq.t -> Sq.t -> t -> (Sq.t list, illegal_move) result
    = fun source target board ->
      let source_coord = Square.coord source
      and target_coord = Square.coord target
      in
      match path_between source_coord target_coord board with
      | Some path -> Ok path
      | None      -> Error (Invalid_move (source, target))

  (** [path_from_valid_piece color source_coord target_coord board] is the [Ok
      (path:Square.t list)] between the squares designated by the [source_coord]
      and [target_coord], if valid, or else an [Error (bad:bad_mode)], where
      [bad.reason] is determined by [square_with_amazon_of_color] or is
      [Invalid_move (source, target)] if there is not a straight path
      between [source:Square.t] and [target:Square.t] *)
  let path_from_valid_piece
  : Pc.color -> Coord.t -> Coord.t -> t -> (Sq.t list, bad_move) result
  = fun color source_coord target_coord board ->
    Result.map_error ~f:(fun reason -> {board; reason})
      begin
        let open Result.Monad_infix in
        square_with_amazon_of_color color @@ square board source_coord
        >>= fun source -> free_square     @@ square board target_coord
        >>= fun target -> valid_path source target board
      end

  (** [clear_path_from_valid_piece color source target board] is an Ok list of
      squares describing the path from [source] to [target], if the path is
      clear and the piece at source is an amazon. *)
  let clear_path_from_valid_piece
    : Pc.color -> Coord.t -> Coord.t -> t -> (Sq.t list, bad_move) result
    = fun color source target board ->
      let open Result.Monad_infix in
      path_from_valid_piece color source target board
      >>= only_empty_squares

  (** [select color source board] is an [Ok board':Board.t] if the [sq:Sq.t]
      selected by [source] is contains a [Piece.Amazon] of [color], or else,
      it is an [Error {reason; board}], with the reason determined by
      [square_with_amazon_of_color]. *)
  let select
    : Pc.color -> Coord.t -> t -> result_of_move
    = fun color source board ->
      let sq = square board source in
      match square_with_amazon_of_color color sq with
      | Ok _         -> Ok board
      | Error reason -> Error {board; reason}

  (** [fire color source target board] is an [Ok board':Board.t], with a
      [Piece.Arrow] of the appropriate [color] placed at the square indicated by
      [target] on the [board] if possible, or an [Error ({board:
      reason}:bad_mode)] where [reason:illegal_move] explains why the move isn't
      possible. *)
  let fire
    : Pc.color -> Coord.t -> Coord.t -> t -> result_of_move
    = fun color source target board ->
      let open Result.Monad_infix in
      clear_path_from_valid_piece color source target board
      >>= fun _ -> place target Pc.(make color Arrow) board

  (** [move color source target board] is an [Ok board':Board.t], with the
      [Piece.Amazon] at the square indicated by [source] moved to the square
      indicated by [target] on [board] if possible, or an [Error ({board:
      reason}:bad_mode)] where [reason:illegal_move] explaining why the move
      isn't possible. *)
  let move
    : Pc.color -> Coord.t -> Coord.t -> t -> result_of_move
    = fun color source target board ->
      let open Result.Monad_infix in
      clear_path_from_valid_piece color source target board
      >>= fun _                -> remove source board
      >>= fun (amazon, board') -> place target amazon board'
end

module Turn = struct

  (** The stages of a turn

      - [Selecting]: Picking a [Piece.Amazon] to move
      - [Moving]: Picking a free [sq:Sq.t] to move to
      - [Firing]: Picking a free [sq:Sq.t] in which to fire a [Piece.Arrow] *)
  type stage =
    | Selecting
    | Moving
    | Firing
  (* [@@deriving show {with_path = false}, yojson] *)

  type t =
    { color  : Piece.color
    ; stage  : stage
    ; source : Coord.t option
    ; board  : Board.t }
  (* [@@deriving show, yojson] *)

  exception Invalid_transition of Coord.t * t

  type result = (t, Board.bad_move) Result.t

  let is_colors_turn
    : Piece.color -> t -> bool
    = fun c {color} -> color = c

  let first : t =
    { color  = Piece.White
    ; stage  = Selecting
    ; source = None
    ; board  = Board.setup }

  let next_color
    : Piece.color -> Piece.color = function
    | Piece.White -> Piece.Black
    | Piece.Black -> Piece.White

  let next_stage
    : stage     -> stage = function
    | Selecting -> Moving
    | Moving    -> Firing
    | Firing    -> Selecting

  (** An action changes a turn, altering its state and advancing its stage,
      based on the supplied coordinates *)
  type action = Coord.t -> t -> result

  open Result.Monad_infix

  (** [select source turn] adds the [source:Coord.t] to the [turn],
      if it designates a [sq:Square.t] with a [Piece.Amazon] of the appropriate
      color, and advances to [Moving:stage] *)
  let select : action
    = fun source turn ->
      match turn with
      | { stage = Selecting; source = None; color; board } ->
        Board.select color source board
        >>= fun _ -> Result.Ok
          { turn with source = Some source
                    ; stage  = next_stage turn.stage }
      | _ -> raise (Invalid_transition (source, turn))

  (** [move target turn] moves the [Piece.Amazon] at the [sq:Square.t]
      designated by [turn.source:Coord.t] to [target] *)
  let move : action
    = fun target turn ->
      match turn with
      | { stage = Moving; source = Some source; color; board } ->
        Board.move color source target board
        >>= fun board -> Result.Ok
          { turn with board
                    ; stage  = next_stage turn.stage
                    ; source = Some target }
      | _ -> raise (Invalid_transition (target, turn))

  (** [fire target turn] fires a [Piece.Arrow] from the [sq:Square.t]
      designated by [turn.source:Coord.t] to [target] *)
  let fire : action
    = fun target turn ->
      match turn with
      | { stage = Firing; source = Some source; color; board } ->
        Board.fire color source target board
        >>= fun board -> Result.Ok
          { board
          ; color  = next_color color
          ; stage  = next_stage turn.stage
          ; source = None }
      | _ -> raise (Invalid_transition (target, turn))

  let next : action
    = fun coord turn ->
      let action = match turn.stage with
        | Selecting -> select
        | Moving    -> move
        | Firing    -> fire
      in
      action coord turn
end

exception Game

type t = Turn.t list
(* [@@deriving show, yojson] *)

(** [turn game] is the current [Turn.t] *)
let turn
  : t -> Turn.t = function
  | t :: _ -> t
  | _      -> raise Game

(** [board game] is the current [Board.t] *)
let board
  : t -> Board.t =
  fun game -> (turn game).Turn.board

(** [turn_color game] is the color whose turn it is *)
let turn_color
  : t -> Piece.color =
  fun game -> (turn game).Turn.color

(** Functions that map into parts of games *)
module Map = struct
  let board
    : f:(Board.t -> Board.t) -> t -> t
    = fun ~f -> function
    | turn :: turns -> {turn with board = f turn.board} :: turns
    | _             -> raise Game
end

module Update = struct
  module Sq = Square
  module Pc = Piece
  module Bd = Board

  exception Update_invalid of string

  type state =
    { game : t
    ; id   : int }
  (* [@@deriving show, yojson] *)

  type result = (state, Board.bad_move) Result.t

  (** Start a new game with the [id] *)
  let start
    : int -> result
    = fun id ->
      Result.Ok { game = [Turn.first]
                ; id }

  (** Makes the only possible move and advances the board to the next state *)
  let move
    : Coord.t -> state -> result
    = fun coord state ->
      match state with
      | { game = [] } -> raise (Update_invalid "no turns in game")
      | { game = (turn :: _) as turns } ->
        let open Result.Monad_infix in
        Turn.next coord turn >>= fun turn' ->
        Result.Ok { state with game = turn' :: turns }
end
