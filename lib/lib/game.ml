open Core

let (%) f g = fun x -> f (g x)

module Coord = struct
  type t =
    { x : int
    ; y : int }
  [@@deriving show {with_path = false}, yojson]

  let min a b = let (x, y) = min (a.x, a.y) (b.x, b.y) in {x; y}
  let max a b = let (x, y) = max (a.x, a.y) (b.x, b.y) in {x; y}

  (* let show {x} *)
  module Of = struct
    let pair (x, y) = {x; y}
  end
  module To = struct
    let pair {x; y} = (x, y)
  end
end

module Piece = struct
  type kind =
    | Amazon
    | Arrow
  [@@deriving show {with_path = false}, yojson]
  type color =
    | Black
    | White
  [@@deriving show {with_path = false}, yojson]
  type t =
    { color : color
    ; kind  : kind }
  [@@deriving show, yojson]

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
  type t =
    { coord : Coord.t
    ; piece : Piece.t option}
  [@@deriving show, yojson]

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
  [@@deriving show, yojson]

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
              ~f:(fun y -> Coord.{x; y}))
      |> List.concat
    in
    List.map coords
      ~f:Sq.empty

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

      - [Empty source] if the [sq] is unoccupied
      - [Invalid_piece source] if the [sq] does not contain a [Piece.Amazon]
      - [Wrong_color source] if the piece on [sq] is not of [color] *)
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

  let clear_path_from_valid_piece
    : Pc.color -> Coord.t -> Coord.t -> t -> (Sq.t list, bad_move) result
    = fun color source target board ->
      let open Result.Monad_infix in
      path_from_valid_piece color source target board
      >>= only_empty_squares

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

  type stage =
    | Select of Piece.kind
    | Move
    | Fire
  [@@deriving show {with_path = false}, yojson]

  type t =
    { color : Piece.color
    ; stage : stage
    ; board : Board.t }
  [@@deriving show, yojson]

  let is_colors_turn
    : Piece.color -> t -> bool
    = fun c {color} -> color = c

  let first : t =
    { color = Piece.White
    ; stage = Select Piece.Amazon
    ; board = Board.setup }

  let switch
    : Piece.color -> Piece.color = function
    | Piece.White -> Piece.Black
    | Piece.Black -> Piece.White

  let next
    : t -> Board.t -> t
    = fun turn board ->
      let (stage, color) = match turn.stage with
        | Select Piece.Amazon -> Move, turn.color
        | Select Piece.Arrow  -> Fire, turn.color
        | Move -> Fire, turn.color
        | Fire -> Move, switch turn.color
      in
      {color; stage; board}

  open Result.Monad_infix

  type action = t -> Coord.t -> Coord.t -> (t, Board.bad_move) result

  let move : action
    = fun turn source target ->
      Board.move turn.color source target turn.board
      >>= fun board -> Result.Ok {turn with board}

  let fire : action
    = fun turn source target ->
      Board.fire turn.color source target turn.board
      >>= fun board -> Result.Ok (next turn board)
end

exception Game

type t = Turn.t list
[@@deriving show, yojson]

let board
  : t -> Board.t = function
  | {board} :: _ -> board
  | _            -> raise Game

let turn
  : t -> Turn.t = function
  | t :: _ -> t
  | _      -> raise Game

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

  exception Update_invalid

  type state =
    { game   : t
    ; source : Coord.t option
    ; target : Coord.t option
    ; id     : int }
  [@@deriving show, yojson]

  type result = (state, Board.bad_move) Result.t

  type msg =
    | Start  of int
    | Select of Coord.t * state
    | Fire   of state
    | Move   of state
  [@@deriving show, yojson]

  let start
    : int -> result
    = fun id ->
      Result.Ok { game   = [Turn.first]
                ; source = None
                ; target = None
                ; id }

  let select
    : Coord.t -> state -> result
    = fun source state ->
      let game = state.game
      in
      let board = board game
      and color = turn_color game
      and sq = game |> board |> fun b -> Bd.square b source
      in
      let square_is_amazon = Sq.piece_is ~f:Pc.is_amazon sq
      and amazon_is_color  = Sq.piece_is ~f:(Pc.is_color color) sq
      in
      if not square_is_amazon then
        Error Bd.{board; reason = Invalid_piece sq}
      else
      if not amazon_is_color then
        Error Bd.{board; reason = Wrong_color sq}
      else
        Ok {state with source = Some source}

  let send
    : msg -> result =
    let open Result.Monad_infix in
    let take action state =
      match state with
      | {game   = []}
      | {source = None}
      | {target = None} ->
        raise Update_invalid
      | { game = (turn :: _) as turns
        ; source = Some source
        ; target = Some target } ->
        action turn source target >>= fun turn' ->
        Result.Ok { state
                    with game   = turn' :: turns
                       ; source = None
                       ; target = None }
    in
    function
    | Start id -> start id
    | Select (source, state) -> select source state
    | Fire state -> take Turn.fire state
    | Move state ->
      (* The source of arrow firing is always the target of the preceding move*)
      take Turn.move state >>| fun state' ->
      {state' with source = state.target}

end
