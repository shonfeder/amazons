open Core
module T = Tyxml

module Coord = Game.Coord
module Board = Game.Board
module Pc    = Game.Piece
module Sq    = Game.Square

module Is = struct
  let even n = (n mod 2 = 0)
  let odd  n = not (even n)
end

module Text = struct
  module To = struct
    (* let coord = Game.read_coord *)
  end
  module Of = struct
    let coord = Game.Coord.show
  end
end

let (%) f g x = f (g x)

module Classes = struct

  (* type t = [`Class] T.Html.attrib *)

  let classes = T.Html.a_class

  module Name = struct
    module Piece = struct
      let t = "piece"
      let color
        : Pc.color -> string = function
        | Pc.Black -> "black"
        | Pc.White -> "white"
      let kind
        : Pc.kind   -> string = function
        | Pc.Arrow  -> "arrow"
        | Pc.Amazon -> "amazon"
    end
    module Square = struct
      let t = "square"
      let color
        : Sq.t -> string
        = fun Sq.{coord={x;y}} ->
          if Is.odd (x - y)
          then "black"
          else "white"
      let occupied
        : Sq.t -> string = function
        | Sq.{piece=Some _} -> "occupied"
        | Sq.{piece=None}   -> "empty"
    end
    module Coord = struct let t = "coord" end
    module Board = struct let t = "board" end
  end

  let piece
    (* : Pc.t -> t *)
    = fun Pc.{color; kind} -> classes
        [ Name.Piece.t
        ; Name.Piece.color color
        ; Name.Piece.kind kind ]

  let square
    (* : Sq.t -> t *)
    = fun square -> classes
        [ Name.Square.t
        ; Name.Square.color square
        ; Name.Square.occupied square]

  let board
    = fun board -> classes
        [ Name.Board.t ]

  let coord
    = fun coord -> classes
        [ Name.Coord.t ]
end

module ID = struct
  type 'a t = 'a T.Html.attrib
  let id = T.Html.a_id

  let square
    : Sq.t -> 'a t
    = id % Text.Of.coord % Sq.coord
end

module Html = struct
  module Html = T.Html
  (* XXX For some reason Html_types are unavailable in shared code? *)
  (* module T    = Html_types *)

  let map = List.map

  type 'a t = 'a Html.elt
  exception Rendering

  let piece
    (* : Pc.t -> [> T.div ] t *)
    = fun pc ->
      Html.div
        ~a:[Classes.piece pc]
        []

  let coord
    (* : (int * int) -> [> T.div ] t *)
    = fun coord ->
      let open Html in
      div
        ~a:[Classes.coord coord]
        [p [pcdata (Text.Of.coord coord)]]

  let square
    (* : Sq.t -> [> T.td ] t *)
    = fun sq ->
      let sq_coord = Sq.coord sq in
      (* let coordstr = Text.Of.coord sq_coord in (\* XXX *\) *)
      let coord   = coord @@ sq_coord
      in
      let id      = ID.square sq
      and classes = Classes.square sq
      and content = match Sq.(sq.piece) with
        | None    -> [coord]
        | Some pc -> [coord; piece pc]
      in
      Html.td
        ~a:[id; classes]
        content

  let row
    (* : Sq.t list -> [> T.tr ] t *)
    = fun (sqs:Sq.t list) ->
      let cells = List.map ~f:square sqs in
      Html.tr cells

  let board
    (* : Board.t -> [> T.table] t *)
    = fun board ->
      let classes = Classes.board board in
      let range   = List.range ~start:`inclusive ~stop:`inclusive 0 9 in
      let xs = range
      and ys = range
      in
      let nth_row y =
        let f x = Coord.{x; y} in
        List.map ~f xs
      in
      let row_of_squares coords =
        let f = Board.square board in
        List.map ~f coords
      in
      let rows_of_squares =
        let f = row_of_squares % nth_row in
        List.map ~f ys |> List.rev
      in
      let rows_of_cells =
        List.map ~f:row rows_of_squares
      in
      Html.table
        ~a:[classes]
        rows_of_cells

  let game
    (* : Game.t -> [> T.table] t *)
    = function
    | (t :: turns) -> board Game.Turn.(t.board)
    | [] -> raise Rendering

end
