open Core

let (%) f g = fun x -> f (g x)

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

  let piece_is
    : f:(Piece.t -> bool) -> t -> bool
    = fun ~f:f square -> Option.exists ~f square.piece
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
  : Pc.color -> coord -> coord -> t -> (Sq.t list, bad_move) result
  = fun color source_coord target_coord board ->
    Result.map_error ~f:(fun reason -> {board; reason})
      begin
        let open Result.Monad_infix in
        square_with_amazon_of_color color @@ square board source_coord
        >>= fun source -> free_square     @@ square board target_coord
        >>= fun target -> valid_path source target board
      end

  let clear_path_from_valid_piece
    : Pc.color -> coord -> coord -> t -> (Sq.t list, bad_move) result
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
    : Pc.color -> coord -> coord -> t -> result_of_move
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

  let first : t =
    { color = Piece.White
    ; board = Board.setup }
  [@@deriving yojson]

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

  let start : t = [Turn.first]

  let send
    : msg -> (t, Board.bad_move) result =
    let take action {game; source; target} =
      match game with
      | [] -> raise Update_invalid
      | (turn :: _) as turns ->
        let open Result.Monad_infix in
        action turn source target
        >>= fun turn' -> Result.Ok (turn' :: turns)
    in
    function
    | Start      -> Result.Ok start
    | Move state -> take Turn.move state
    | Fire state -> take Turn.fire state

end
