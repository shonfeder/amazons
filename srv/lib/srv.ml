open Core
open Opium.Std
module T = Tyxml
module H = T.Html

module G = Game

(* AUX *)

let yo_to_ez_json yo =
  yo
  |> Yojson.Safe.to_string
  |> Ezjsonm.from_string

exception Invalid_request of Opium_kernel__Rock.Request.t
exception Game_room_full
exception No_game of int

type game_state = Game.Update.state

let games : game_state option array =
  Array.init ~f:(fun _ -> None) 1000

let create_game () : G.Update.result =
  let add_new_game id =
    let game_state = G.Update.(send (Start id)) in
    Array.set games id (Result.ok game_state) ;
    game_state
  in
  match Array.findi games ~f:(fun _ -> Option.is_none) with
  | Some (indx, _) -> add_new_game indx
  | None           -> raise Game_room_full

let lookup_game indx : game_state option = games.(indx)
let update_game
  : f:(game_state -> game_state) -> int -> unit
  = fun ~f id ->
    match lookup_game id with
    | None            -> raise (No_game id)
    | Some game_state -> Array.nset games id @@ Some (f game_state)

let set_game_state (G.Update.{id} as state) =
  match lookup_game id with
  | None            -> raise (No_game id)
  | Some game_state -> Array.nset games id @@ Some (state)

module Page = struct
  let default_css = "/assets/css/amazons.css"
  let amazons_js = "/assets/js/amazons.js"

  let to_string html =
    Format.asprintf "%a" (T.Html.pp ()) html

  let elt_to_string html_elmt =
    Format.asprintf "%a" (T.Html.pp_elt ()) html_elmt

  let make ?(title="") ?(css=default_css) content : App.body =
    let titlestr = title in
    `Html
      begin let open T.Html in
        html
          (head
             (title @@ pcdata titlestr)
             [ link ~rel:[`Stylesheet] ~href:css ()
             ; script
                 ~a:[a_src (Xml.uri_of_string amazons_js)]
                 (cdata "")
             ])
          (body
             content)
        |> to_string
      end

  let respond ?(title="") ?(css=default_css) content =
    make ~title ~css content |> respond'

  let page_h1 string = T.Html.(h1 [pcdata string])
  let game_title game_id =
    "New Game of the Amazons: #" ^ string_of_int game_id
  let game_id_data id =
    let id = string_of_int id in
    H.(div ~a:[a_id "game-id"; a_user_data "game-id" id] [])

  let game ?(content=[]) =
    function
    | Error G.Board.{board; reason} ->
      let title = "Bad move!" in
      respond ~title @@
      H.[ h1 [pcdata @@ "Bad Move: " ^ G.Board.show_illegal_move reason ]
        ; Render.Html.board G.Turn.{ board;
                                     (* Junk data *)
                                     color = G.Piece.White } ]
    | Ok G.Update.{game; id} ->
      let title = game_title id in
      respond ~title @@
      [ H.div
          ([ page_h1 title
           ; game_id_data id
           ; H.div
               ~a:[H.a_class ["game-container"]]
               [Render.Html.game game] ]
           @ content )]

  let new_game_link =
    T.Html.(a ~a:[a_href "/game/new"] [pcdata "New Game"])

  let missing_game =
    respond
      T.Html.[div [ h1 [pcdata "That game doesn't exist!"]
                  ; p  [new_game_link]]]
end

module API = struct
  type json_in      = Yojson.Safe.json
  type json_result  = (Yojson.Safe.json, string) result
  type api_response = Opium_kernel.Rock.Response.t Lwt.t

  let of_game_data
    : id:int
      -> data:string
      -> f:(Game.Update.state -> json_in -> json_result)
      -> api_response
    = fun ~id ~data ~f ->
      let game_state = match lookup_game id with
        | None -> raise (No_game id)
        | Some g -> g
      in
      let json_result =
        data
        |> Yojson.Safe.from_string
        |> f game_state
      in
        match json_result with
        | Ok json     -> respond' @@ `String (Yojson.Safe.to_string json)
        | Error err   ->
          let data = Ezjsonm.(dict ["kind", `String "bad_request";
                                    "expected", `String err;
                                    "recieved", `String data] )
          in respond' ~code:`Bad_request (`Json data)

  let update_game_state
    = fun (state_result:G.Update.result) ->
      let code, data = match state_result with
        | Ok state -> set_game_state state;
          let board_html = state.game |> Render.Html.game |> Page.elt_to_string
          in
          (`Accepted, `String board_html)
        | Error bad_move ->
          let bad_move = G.Board.bad_move_to_yojson bad_move |> yo_to_ez_json
          in
          (`Conflict, bad_move)
      in
      respond' ~code (`Json (Ezjsonm.dict ["data", data]))
end

module Param = struct
  let to_id req =
    try param req "id" |> int_of_string
    with Failure _ -> raise (Invalid_request req)

  let to_game req =
    let id = req |> to_id in
    match lookup_game id with
    | None      -> raise (No_game id)
    | Some game -> game

  let to_msg req =
    param req "msg"
    |> Yojson.Safe.from_string
    |> Game.Update.msg_of_yojson

  let to_coord req =
    let coord_str = param req "coord" in
    coord_str
    |> Yojson.Safe.from_string
    |> Game.Coord.of_yojson
end

let home = get "/"
    begin fun req ->
      let open T.Html in
      Page.respond @@
      [div
         [ h1 [pcdata "The Game of the Amazons"]
         ; ul [li [Page.new_game_link]]
         ]
      ]
    end

let new_game = get "/game/new"
    begin fun _req ->
      (* TODO redirect to game/:id page *)
      Page.game (create_game ())
    end

let game = get "/game/:id"
    begin fun req ->
      match Param.to_game req with
      | game                  -> Page.game (Ok game)
      | exception (No_game _) -> Page.missing_game
    end

let move_api = get "/api/game/move/:id/:coord"
    begin fun req ->
      let coord = match Param.to_coord req with
        | Ok coord -> coord
        | Error _  -> raise (Invalid_request req)
      in
      match Param.to_game req with
      | state -> G.Update.move state coord |> API.update_game_state
      | exception (No_game _) -> Page.missing_game
    end

(** TODO reply with json packages including the html string as data*)
let game_board_html_api = get "/api/game/board/:id"
    begin fun req ->
      match Param.to_game req with
      | G.Update.{game} ->
        let html_string = Render.Html.game game |> Page.elt_to_string in
        respond' (`String html_string)
      | exception (No_game _) -> Page.missing_game
    end

let game_state_json_api = get "/api/game/state/:id"
    begin fun req ->
      match Param.to_game req with
      | game                  ->
        respond' @@ `Json (game
                           |> G.Update.state_to_yojson
                           |> yo_to_ez_json)
      | exception (No_game _) -> Page.missing_game
    end

(* Return an example json representation of a message for the given game ID *)
let example_api = get "/api/example/msg/:id/:msg_kind"
    begin fun req ->
      let msg_kind = param req "msg_kind" in
      try
        let state = Param.to_game req in
        let open G.Update in
        let msg_json =
          begin
            let coord = G.Coord.Of.pair (0, 0) in
            match msg_kind with
            | "start"  -> Start state.id
            | "select" -> Select (coord, state)
            | "fire"   -> Fire   (coord, state)
            | "move"   -> Move   (coord, state)
            | _        -> raise (Invalid_request req)
          end
          |> msg_to_yojson
          |> yo_to_ez_json
        in
        respond' (`Json msg_json)
      with (No_game _) -> Page.missing_game
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
  |> move_api
  |> game_board_html_api
  |> game_state_json_api
  |> example_api
  |> App.run_command
