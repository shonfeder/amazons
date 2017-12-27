open Core
open QCheck
open Game

exception Testing

module GameGen = struct
  let coord
    : (int * int) Gen.t
    = Gen.(pair (0 -- 9) (0 -- 9))

  module Piece = struct
    let kind
      : Piece.kind Gen.t
      = Gen.oneofl [Piece.Amazon; Piece.Arrow]

    let color
      : Piece.color Gen.t
      = Gen.oneofl [Piece.Black; Piece.White]

    let t
      : Piece.t Gen.t
      = fun rand ->
        { color = color rand
        ; kind  = kind rand }
  end

  module Square = struct
    let t
      : Square.t Gen.t
      = fun rand ->
        { coord = coord rand
        ; piece = Gen.opt Piece.t rand }
  end

  module Board =
  struct

    let place_piece_on_sq
      : Game.Piece.t -> Game.Square.t -> Game.Square.t
      = let open Game in
      fun piece Square.{coord} -> Square.make coord piece

    let amazons =
      let open Game in
      List.init 4 (fun _ -> Piece.(make Black Amazon)) @
      List.init 4 (fun _ -> Piece.(make White Amazon))

    let t
      : Board.t Gen.t
      = fun rand ->
        let num_arrows = Gen.(0 -- 45) rand in
        let squares = Gen.shuffle_l Board.empty rand in
        let (amazon_sqs, rest)  = List.split_n squares 8 in
        let (blk_arr_sqs, rest) = List.split_n rest num_arrows in
        let (wht_arr_sqs, rest) = List.split_n rest num_arrows in
        let amazon_sqs =
          List.map2_exn ~f:place_piece_on_sq amazons amazon_sqs
        in
        let blk_arr_sqs =
          List.map ~f:(place_piece_on_sq Game.Piece.(black Arrow)) blk_arr_sqs
        in
        let wht_arr_sqs =
          List.map ~f:(place_piece_on_sq Game.Piece.(white Arrow)) wht_arr_sqs
        in
        amazon_sqs @ blk_arr_sqs @ wht_arr_sqs @ rest
  end

end

module Arbitrary = struct
  let coord = pair (0 -- 9) (0 -- 9)
  module Piece = struct
    let kind
      : Piece.kind arbitrary
      = make ~print:Piece.show_kind GameGen.Piece.kind

    let color
      : Piece.color arbitrary
      = make ~print:Piece.show_color GameGen.Piece.color

    let t
      : Piece.t arbitrary
      = make ~print:Piece.show GameGen.Piece.t
  end

  module Square = struct
    let t
      : Square.t arbitrary
      = make ~print:Square.show GameGen.Square.t
  end

  module Board = struct
    let t
      : Board.t arbitrary
      = make ~print:Board.show GameGen.Board.t
  end

end

module Aux = struct
  let place_exn coord piece board =
    match Board.(place coord piece empty) with
    | Error _ -> raise Testing (* This should be impossible *)
    | Ok board -> board
end

let tests = [
  Test.make
    ~name:"reading coords"
    Arbitrary.coord
    begin fun coord ->
      let coord_read = Game.read_coord @@ Game.show_coord coord in
      coord_read = coord
    end
  ;
  Test.make
    ~name:"possible kinds of a piece"
    Arbitrary.Piece.kind
    begin fun kind ->
      Piece.(kind = Amazon || kind = Arrow)
    end
  ;
  Test.make
    ~name:"possible colors of a piece"
    Arbitrary.Piece.color
    begin fun color ->
      Piece.(color = Black || color = White)
    end
  ;
  Test.make
    ~name:"possible pieces"
    Arbitrary.Piece.t
    begin let open Piece in
      fun {color; kind} ->
        (color = Black || color = White)
        &&
        (kind = Amazon || kind = Arrow)
    end
  ;
  Test.make
    ~name:"empty squares never have pieces on them"
    Arbitrary.coord
    begin let open Square in
      fun coord ->
        (empty coord).piece = None
    end
  ;
  Test.make
    ~name:"all empty squares can be selected with valid coords on an empty board"
    Arbitrary.coord
    begin fun coord ->
      Square.is_empty (Board.square Board.empty coord)
    end
  ;
  Test.make
    ~name:"getting a square with invalid coords raises"
    (pair (pair (10 -- 100) (10 -- 100)) Arbitrary.Board.t)
    begin
      fun (coord, board) ->
        match Board.square board coord with
        | exception Not_found -> true
        | _ -> false
    end
  ;
  Test.make
    ~name:"select_square takes a square and leaves the rest"
    Arbitrary.(pair coord Board.t)
    begin fun (coord, board) ->
      let (sq, rest) = Board.select_square coord board in
      List.length rest = 99
    end
  ;
  Test.make
    ~name:"can retrieve piece placed on board"
    Arbitrary.(pair coord Piece.t)
    begin fun (coord, piece) ->
      let board = Board.empty in
      match Board.place coord piece board with
      | Error _  -> false
      | Ok board ->
        let piece' = Board.square board coord |> Square.piece in
        piece' = Some piece
    end
  ;
  Test.make
    ~name:"placing piece on an occupied square returns an Occupied bad_move"
    Arbitrary.(pair coord Piece.t)
    begin fun (coord, piece) ->
      let board = Aux.place_exn coord piece Board.empty in
      let sq = Board.square board coord in
      match Board.place coord piece board with
      | Ok _ -> false
      | Error Board.{reason} ->
        reason = Board.(Occupied sq)
    end
  ;
  Test.make
    ~name:"removing pieces returns correct values"
    Arbitrary.(pair coord Board.t)
    begin fun (coord, board) ->
      let Square.{piece} as sq = Board.square board coord
      in
      let piece_is_arrow = Option.exists ~f:Piece.is_arrow piece in
      let square_was_empty sq' = Square.is_empty sq && Square.is_empty sq' in
      let piece_is_removed piece' board' =
        (Some piece') = piece && Square.is_empty (Board.square board' coord)
      in
      match Board.remove coord board with
      | Error {reason = Invalid_piece _ } -> piece_is_arrow
      | Error {reason = Empty sq'}        -> square_was_empty sq'
      | Ok (piece', board')               -> piece_is_removed piece' board'
      | _                                 -> false
    end
  ;
  Test.make
    ~name:"board setup is correct"
    unit
    begin fun () ->
      let black_positions = [(6,9) ; (9,6) ; (6,0) ; (9,3)] in
      let white_positions = [(0,6) ; (3,9) ; (0,3) ; (3,0)] in
      let sq_coords_in coord_list Square.{coord} =
        List.mem ~equal:((=)) coord_list coord in
      let is_black_sq sq = sq_coords_in black_positions sq in
      let is_white_sq sq = sq_coords_in white_positions sq in
      let has_white_piece sq = Option.exists ~f:Piece.is_white @@ Square.piece sq in
      let has_black_piece sq = Option.exists ~f:Piece.is_black @@ Square.piece sq in
      let board = Board.setup in
      let (black_squares, rest) = List.partition_tf ~f:is_black_sq board in
      let (white_squares, rest) = List.partition_tf ~f:is_white_sq rest in
      List.for_all white_squares ~f:has_white_piece &&
      List.for_all black_squares ~f:has_black_piece &&
      List.for_all rest ~f:Square.is_empty
    end
]

let () = QCheck_runner.run_tests_main tests
