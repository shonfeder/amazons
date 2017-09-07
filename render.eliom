module Board = Game.Board
module Pc    = Game.Piece
module Sq    = Game.Square

let (%) = Batteries.(%)

module Is = struct
  let even n = (n mod 2 = 0)
  let odd  n = not (even n)
end

module List = struct
  include List
  let singleton x = [x]
end

module Classes = struct

  (* type t = [`Class] Eliom_content.Html.D.attrib *)

  let classes = Eliom_content.Html.D.a_class

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
        = fun Sq.{coord=(x,y)} ->
          if Is.odd (x - y)
          then "black"
          else "white"
      let occupied
        : Sq.t -> string = function
        | Sq.{piece=Some _} -> "occupied"
        | Sq.{piece=None}   -> "empty"
    end
    module Board = struct
      let t = "board"
    end
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
end

module ID = struct
  type 'a t = 'a Eliom_content.Html.D.attrib
  let id = Eliom_content.Html.D.a_id

  let square
    : Sq.t -> 'a t
    = fun Sq.{coord=(x, y)} -> id @@
      Printf.sprintf "%n-%n" x y
end

module Html = struct
  module Html = Eliom_content.Html.D
  module T    = Html_types

  let map = List.map

  type 'a t = 'a Html.elt

  let piece
    : Pc.t -> [> T.div ] t
    = fun pc ->
      Html.div
        ~a:[Classes.piece pc]
        []

  let square
    : Sq.t -> [> T.td ] t
    = fun sq ->
      (* XXX *)
      let coord_str = let (x, y) = Sq.(sq.coord) in
        Printf.sprintf "%n-%n" x y
      in
      let id_str  = Html.(p [pcdata coord_str])
      in
      (* XXX *)
      let id      = ID.square sq
      and classes = Classes.square sq
      and content = match Sq.(sq.piece) with
        | None    -> [id_str]
        | Some pc -> [piece pc;
                      (*XXX*) id_str]
      in
      Html.td
        ~a:[id; classes]
        content

  let row
    : Sq.t list -> [> T.tr ] t
    = fun (sqs:Sq.t list) ->
      let cells = map square sqs in
      Html.tr cells

  let board
    : Board.t -> [> T.table] t
    = fun board ->
      let classes = Classes.board board in
      let range = Aux.range 0 9 in
      let xs = range
      and ys = range
      in
      let nth_row y = map (fun x -> (y, x)) xs in
      let row_of_squares coords = map (Board.square board) coords
      in
      let rows_of_squares = map (row_of_squares % nth_row) ys |> List.rev in
      let rows_of_cells   = map row rows_of_squares
      in
      Html.table
        ~a:[classes]
        rows_of_cells

end
