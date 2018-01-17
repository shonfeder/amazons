open Core
open Opium.Std
module T = Tyxml
module H = T.Html

module G = Game

exception Invalid_request of Opium_kernel__Rock.Request.t
exception Game_room_full

type game_id = int
type game_state =
  { id       : game_id
  ; game     : Game.t
  ; selected : Game.Coord.t option }

let games : game_state option array ref =
  ref (Array.init ~f:(fun _ -> None) 1000)

let create_game () : game_state =
  let game = Game.Update.start
  and selected = None
  in
  let add_new_game id =
    let game_state = {id; game; selected} in
    Array.set (!games) id (Some game_state) ;
    game_state
  in
  match Array.findi (!games) ~f:(fun _ -> Option.is_none) with
  | Some (indx, _) -> add_new_game indx
  | None           -> raise Game_room_full

let lookup_game indx : game_state option = (!games).(indx)


module Page = struct
  let default_css = "/assets/css/amazons.css"

  let to_string html =
    Format.asprintf "%a" (T.Html.pp ()) html

  let make ?(title="") ?(css=default_css) content : App.body =
    let titlestr = title in
    `Html
      begin let open T.Html in
        html
          (head
             (title @@ pcdata titlestr)
             [link ~rel:[`Stylesheet] ~href:css ()])
          (body
             content)
        |> to_string
      end

  let respond ?(title="") ?(css=default_css) content =
    make ~title ~css content |> respond'

  let page_h1 string = T.Html.(h1 [pcdata string])
  let game_title game_id =
    "New Game of the Amazons: #" ^ string_of_int game_id

  let game ?(content=[]) {game; id} =
    let title = game_title id in
    respond ~title @@
    [ H.div
        ([ page_h1 title
         ; Render.Html.game game ]
         @ content )]
end

let new_game_link =
  T.Html.(a ~a:[a_href "/game/new"] [pcdata "New Game"])

let home = get "/"
    begin fun req ->
      let open T.Html in
      Page.respond @@
      [div
         [ h1 [pcdata "The Game of the Amazons"]
         ; ul [li [new_game_link]]
         ]
      ]
    end

let new_game = get "/game/new"
    begin fun _req ->
      (* TODO redirect to game/:id page *)
      Page.game @@ create_game ()
    end

let game = get "/game/:id"
    begin fun req ->
      let game_id =
        try param req "id" |> int_of_string
        with Failure _ -> raise (Invalid_request req)
      in
      let open T.Html in
      match lookup_game game_id with
        | Some game_state -> Page.game game_state
        | None            -> Page.respond
                               [div [ h1 [pcdata "That game doesn't exist!"]
                                    ; p  [new_game_link]]]
    end

let select_amazon = get "/game/:id/select/amazon/:color/:coord"
    begin fun req ->
      let coord = param req "coord" |> Json.To.coord in
      let id    = param req "id"    |> int_of_string in
      let game_state = match lookup_game id with
        | Some g -> g
        | None   -> raise (Invalid_request req) (*XXX*)
      in
      let color = match param req "color" |> G.Piece.color_of_string with
        | Some c -> c
        | None   -> raise (Invalid_request req)
      in
      let board = Game.board game_state.game in
      let f pc = G.Piece.(is_amazon pc && is_color color pc) in
      if G.Square.piece_is ~f @@ G.Board.square board coord
      then
        Page.game
          game_state
          ~content:H.[p [pcdata @@ Render.Text.Of.coord coord]]
      else
        Page.respond
          H.[ h1 [pcdata "Invalid selection"]
            ; p  [pcdata @@ Printf.sprintf
                    "Color is %s" (Game.Piece.show_color color)]
            ; p  [pcdata @@ Printf.sprintf
                    "x is: %i and y is: %i"
                    coord.x coord.y]]
    end

let resources = middleware @@
  Middleware.static
    ~local_path:"/Users/sf/Dropbox/Programming/amazons/srv/static" (* XXX *)
    ~uri_prefix:"/assets"

let _ =
  App.empty
  |> resources
  |> home
  |> game
  |> new_game
  |> select_amazon
  |> App.run_command
