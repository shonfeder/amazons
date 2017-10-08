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

module Text = struct
  let coord (x, y) = Printf.sprintf "%n-%n" x y
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
  type 'a t = 'a Eliom_content.Html.D.attrib
  let id = Eliom_content.Html.D.a_id

  let square
    : Sq.t -> 'a t
    = id % Text.coord % Sq.coord
end

module Html = struct
  module Html = Eliom_content.Html.D
  module T    = Html_types

  let map = List.map

  type 'a t = 'a Html.elt
  exception Rendering

  let piece
    : Pc.t -> [> T.div ] t
    = fun pc ->
      Html.div
        ~a:[Classes.piece pc]
        []

  let coord
    : (int * int) -> [> T.div ] t
    = fun coord ->
      let open Html in
      div
        ~a:[Classes.coord coord]
        [p [pcdata (Text.coord coord)]]

  let square
    : Sq.t -> [> T.td ] t
    = fun sq ->
      let coordstr = Text.coord @@ Sq.coord sq in (* XXX *)
      let coord   = coord @@ Sq.coord sq
      in
      let id      = ID.square sq
      and classes = Classes.square sq
      (* TODO Event handler should feed data into client side functions *)
      and onclick = Html.a_onclick [%client
        fun _ -> Dom_html.window##alert(Js.string ("Square " ^ ~%coordstr))
        ]
      and content = match Sq.(sq.piece) with
        | None    -> [coord]
        | Some pc -> [coord; piece pc]
      in
      Html.td
        ~a:[onclick; id; classes]
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
      let range   = Aux.range 0 9 in
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

  let game
    : Game.t -> [> T.table] t = function
    | (t :: turns) -> board Game.Turn.(t.board)
    | [] -> raise Rendering

end
