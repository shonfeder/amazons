open Core
open QCheck
open Game

module Pc = Piece
module Sq = Square
module Bd = Board

exception Testing

let check
  : bool -> name:string -> bool
  = fun test ~name ->
    test || (Printf.printf "check failed: %s\n" name; false)

module GameGen = struct
  let coord
    : (int * int) Gen.t
    = Gen.(pair (0 -- 9) (0 -- 9))

  module Piece = struct
    let kind
      : Pc.kind Gen.t
      = Gen.oneofl [Pc.Amazon; Pc.Arrow]

    let color
      : Pc.color Gen.t
      = Gen.oneofl [Pc.Black; Pc.White]

    let t
      : Pc.t Gen.t
      = fun rand ->
        { color = color rand
        ; kind  = kind rand }
  end

  module Square = struct
    let t
      : Sq.t Gen.t
      = fun rand ->
        { coord = coord rand
        ; piece = Gen.opt Piece.t rand }
  end

  module Board =
  struct

    let place_piece_on_sq
      : Pc.t -> Sq.t -> Sq.t
      = fun piece Sq.{coord} -> Sq.make coord piece

    let amazons =
      List.init 4 (fun _ -> Pc.(make Black Amazon)) @
      List.init 4 (fun _ -> Pc.(make White Amazon))

    let t
      : Bd.t Gen.t
      = fun rand ->
        let num_arrows = Gen.(0 -- 45) rand in
        let squares = Gen.shuffle_l Bd.empty rand in
        let (amazon_sqs, rest)  = List.split_n squares 8 in
        let (blk_arr_sqs, rest) = List.split_n rest num_arrows in
        let (wht_arr_sqs, rest) = List.split_n rest num_arrows in
        let amazon_sqs =
          List.map2_exn ~f:place_piece_on_sq amazons amazon_sqs
        in
        let blk_arr_sqs =
          List.map ~f:(place_piece_on_sq Pc.(black Arrow)) blk_arr_sqs
        in
        let wht_arr_sqs =
          List.map ~f:(place_piece_on_sq Pc.(white Arrow)) wht_arr_sqs
        in
        Gen.shuffle_l (amazon_sqs @ blk_arr_sqs @ wht_arr_sqs @ rest) rand

    let coord_of_amazon_with_color
      : Pc.color -> Bd.t -> coord Gen.t
      = fun color board rand ->
        let is_square_with_amazon_of_color sq =
          Result.is_ok (Bd.square_with_amazon_of_color color sq)
        in
        let squares = List.filter board ~f:is_square_with_amazon_of_color in
        Sq.coord @@ Gen.oneofl squares rand
  end

end

module Arbitrary = struct
  let coord = pair (0 -- 9) (0 -- 9)
  module Piece = struct
    let kind
      : Pc.kind arbitrary
      = make ~print:Pc.show_kind GameGen.Piece.kind

    let color
      : Pc.color arbitrary
      = make ~print:Pc.show_color GameGen.Piece.color

    let t
      : Pc.t arbitrary
      = make ~print:Pc.show GameGen.Piece.t
  end

  module Square = struct
    let t
      : Sq.t arbitrary
      = make ~print:Sq.show GameGen.Square.t
  end

  module Board = struct
    let t
      : Bd.t arbitrary
      = make ~print:Bd.show GameGen.Board.t

    let coord_of_amazon_with_color
      : Pc.color -> Bd.t -> coord arbitrary
      = fun color board -> make
          ~print:show_coord
          (GameGen.Board.coord_of_amazon_with_color color board)
  end
end

module Aux = struct
  let place_exn coord piece board =
    match Bd.(place coord piece empty) with
    | Error _ -> raise Testing (* This should be impossible *)
    | Ok board -> board

  let rec is_ascending_sequence = function
    | x1::x2::xs -> x2 = (x1 + 1) && is_ascending_sequence (x2::xs)
    | _          -> true

  let is_single_value ls =
    1 = List.length @@ Caml.List.sort_uniq compare ls

  let are_in_a_line sqs =
    let coords = List.map ~f:Sq.coord sqs in
    let (xs, ys) = List.unzip coords in
    coords = (List.sort coords ~cmp:compare) &&
    (is_ascending_sequence xs || is_single_value xs) &&
    (is_ascending_sequence ys || is_single_value ys)
end

let piece_tests =
  [
    Test.make
      ~name:"possible kinds of a piece"
      Arbitrary.Piece.kind
      begin fun kind ->
        Pc.(kind = Amazon || kind = Arrow)
      end
    ;
    Test.make
      ~name:"possible colors of a piece"
      Arbitrary.Piece.color
      begin fun color ->
        Pc.(color = Black || color = White)
      end
    ;
    Test.make
      ~name:"possible pieces"
      Arbitrary.Piece.t
      begin fun Pc.{color; kind} ->
        (color = Pc.Black || color = Pc.White) &&
        (kind = Pc.Amazon || kind = Pc.Arrow)
      end
  ]

let square_tests =
  [
    Test.make
      ~name:"empty squares never have pieces on them"
      Arbitrary.coord
      begin fun coord ->
        (Sq.empty coord).piece = None
      end
    ;
    Test.make
      ~name:"all empty squares can be selected with valid coords on an empty board"
      Arbitrary.coord
      begin fun coord ->
        Sq.is_empty @@ Bd.square Bd.empty coord
      end
  ]

let board_tests =
  [
    Test.make
      ~name:"getting a square with invalid coords raises"
      (pair (pair (10 -- 100) (10 -- 100)) Arbitrary.Board.t)
      begin fun (coord, board) ->
        match Bd.square board coord with
        | exception Not_found -> true
        | _                   -> false
      end
    ;
    Test.make
      ~name:"select_square takes a square and leaves the rest"
      Arbitrary.(pair coord Board.t)
      begin fun (coord, board) ->
        let (sq, rest) = Bd.select_square coord board in
        List.length rest = 99
      end
    ;
    Test.make
      ~name:"can retrieve piece placed on board"
      Arbitrary.(pair coord Piece.t)
      begin fun (coord, piece) ->
        let board = Bd.empty in
        match Bd.place coord piece board with
        | Error _  -> false
        | Ok board ->
          let piece' = Bd.square board coord |> Sq.piece in
          piece' = Some piece
      end
    ;
    Test.make
      ~name:"placing piece on an occupied square returns an Occupied bad_move"
      Arbitrary.(pair coord Piece.t)
      begin fun (coord, piece) ->
        let board = Aux.place_exn coord piece Bd.empty in
        let sq = Bd.square board coord in
        match Bd.place coord piece board with
        | Error Bd.{reason} -> reason = Bd.(Occupied sq)
        | Ok _              -> false
      end
    ;
    Test.make
      ~name:"removing pieces returns correct values"
      Arbitrary.(pair coord Board.t)
      begin fun (coord, board) ->
        let Sq.{piece} as sq = Bd.square board coord
        in
        let piece_is_arrow = Option.exists ~f:Pc.is_arrow piece in
        let square_was_empty sq' = Sq.is_empty sq && Sq.is_empty sq' in
        let piece_is_removed piece' board' =
          (Some piece') = piece && Sq.is_empty (Bd.square board' coord)
        in
        match Bd.remove coord board with
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
        let sq_coords_in coord_list Sq.{coord} =
          List.mem ~equal:((=)) coord_list coord in
        let is_black_sq sq = sq_coords_in black_positions sq in
        let is_white_sq sq = sq_coords_in white_positions sq in
        let has_white_piece sq = Option.exists ~f:Pc.is_white @@ Sq.piece sq in
        let has_black_piece sq = Option.exists ~f:Pc.is_black @@ Sq.piece sq in
        let board = Bd.setup in
        let (black_squares, rest) = List.partition_tf ~f:is_black_sq board in
        let (white_squares, rest) = List.partition_tf ~f:is_white_sq rest in
        List.for_all white_squares ~f:has_white_piece &&
        List.for_all black_squares ~f:has_black_piece &&
        List.for_all rest ~f:Sq.is_empty
      end
    ;
    Test.make
      ~name:"line_of_squares only returns squares on a line"
      Arbitrary.(pair coord coord)
      begin fun (source, target) ->
        match Bd.line_of_squares source target Bd.empty with
        | Some squares -> Aux.are_in_a_line squares
        | None         -> true
      end
    ;
    Test.make
      ~name:"only_empty_squares returns correctly on empty and non-empty squares"
      Arbitrary.(pair Board.t int)
      begin fun (board, n) ->
        let sqs = List.take board n in
        match Bd.only_empty_squares sqs with
        | Ok squares ->
          List.for_all squares ~f:Sq.is_empty
        | Error { reason = Blocked occupied_sq } ->
          not (Sq.is_empty occupied_sq)
        | _ ->
          false
      end
    ;
    Test.make
      ~name:"square_with_amazon_of_color returns the correct [Ok]s and [Error]s"
      Arbitrary.(pair Piece.color Square.t)
      begin fun (color', sq) ->
        match Bd.square_with_amazon_of_color color' sq with
        | Error Bd.(Empty         Sq.{piece = None})            -> true
        | Error Bd.(Wrong_color   Sq.{piece = Some Pc.{color}}) -> color <> color'
        | Error Bd.(Invalid_piece Sq.{piece = Some Pc.{kind}})  -> kind  = Pc.Arrow
        | Ok    Sq.{piece = Some  Pc.{kind = Amazon; color}}    -> color = color'
        | _                                                     -> false
      end
    ;
    Test.make
      ~name:"path_from_valid_piece returns correctly on valid and invalid paths"
      Arbitrary.(quad Piece.color coord coord Board.t)
      begin fun (color, source, target, board) ->
        let source_sq = Bd.square board source
        and target_sq = Bd.square board target
        in
        let amazon_of_color p = Pc.(is_amazon p && is_color color p)
        in
        match Bd.path_from_valid_piece color source target board with
        | Ok path ->
          check ~name:"path is ok"
            (Sq.piece_is ~f:amazon_of_color source_sq &&
             Sq.is_empty target_sq)
        | Error {board; reason} ->
          match reason with
          | Bd.Empty _ ->
            check
              ~name:"square is empty"
              (Sq.is_empty source_sq)
          | Bd.Invalid_piece _ ->
            check
              ~name:"piece is invalid"
              (not @@ Sq.piece_is ~f:Pc.is_amazon source_sq)
          | Bd.Wrong_color _ ->
            check
              ~name:"color is wrong"
              (Sq.piece_is ~f:Pc.is_amazon source_sq &&
               not @@ Sq.piece_is ~f:Pc.(is_color color) source_sq)
          | Bd.Occupied _ ->
            check
              ~name:"target is occupied"
              (not @@ Sq.is_empty target_sq)
          | Bd.Invalid_move _ ->
            true (* This should be tested in line_of_squares *)
          | _ ->
            false
      end
    ;
    Test.make
      ~name:"clear_path_from_valid_piece always returns clear ok paths or errors"
      Arbitrary.(quad Piece.color coord coord Board.t)
      begin fun (color, source, target, board) ->
        match Bd.clear_path_from_valid_piece color source target board with
        | Ok path -> List.for_all path ~f:Square.is_empty
        | _       -> true

      end
    ;
    Test.make
      ~name:"fire places an arrow or returns an Error"
      Arbitrary.(quad Piece.color coord coord Board.t)
      begin fun (color', source, target, board) ->
        match Bd.fire color' source target board with
        | Error _  -> true
        | Ok board ->
          match Bd.square board target with
          | Sq.{piece = Some Pc.{kind = Arrow; color}} -> color = color'
          | _                                          -> false
      end
    ;
    Test.make
      ~name:"move moves an amazon or returns an Error"
      Arbitrary.(quad Piece.color coord coord Board.t)
      begin fun (color', source, target, board) ->
        match Bd.move color' source target board with
        | Error _  -> true
        | Ok board ->
          match Bd.square board source, Bd.square board target with
          | Sq.({piece = None}, {piece = Some Pc.{kind = Amazon; color}}) ->
            color = color'
          | _ -> false
      end
  ]

let turn_tests =
  [
    (* TODO *)
  ]

let game_tests =
  [
    Test.make
      ~name:"reading coords"
      Arbitrary.coord
      begin fun coord ->
        let coord_read = read_coord @@ show_coord coord in
        coord_read = coord
      end
  ]

let tests = piece_tests @
            square_tests @
            board_tests @
            turn_tests @
            game_tests

let () = QCheck_runner.run_tests_main tests
