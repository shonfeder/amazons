open Core
open Opium.Std
module T = Tyxml
module H = T.Html

exception Invalid_request of Opium_kernel__Rock.Request.t
exception Game_room_full

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

  let game game game_id =
    let title = game_title game_id in
    respond ~title @@
    [T.Html.div
       [ page_h1 title
       ; Render.Html.game game ]]
end

type game_id = int
let games : Game.t option array ref =
  ref (Array.init ~f:(fun _ -> None) 1000)

let create_game () : Game.t * game_id =
  let new_game = Game.Update.start in
  let add_new_game game_id =
    Array.set (!games) game_id (Some new_game) ;
    (new_game, game_id)
  in
  match Array.findi (!games) ~f:(fun _ -> Option.is_none) with
  | Some (indx, _) -> add_new_game indx
  | None           -> raise Game_room_full

let lookup_game indx : Game.t option = (!games).(indx)

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
    begin fun req ->
      let game, game_id = create_game () in
      let open T.Html in
      (* TODO redirect to game/:id page *)
      Page.game game game_id
    end

let game = get "/game/:id"
    begin fun req ->
      let game_id =
        try param req "id" |> int_of_string
        with Failure _ -> raise (Invalid_request req)
      in
      let open T.Html in
      match lookup_game game_id with
        | Some game -> Page.game game game_id
        | None      -> Page.respond [div [ h1 [pcdata "That game doesn't exist!"]
                                         ; p  [new_game_link]]]
    end

(* (\* select an amazon for movement *\)
 * let select_amazon = get "/game/:id/select/:color/:coord"
 *     begin fun req ->
 *       let color = match param req "color" |> Game.Piece.color_of_string with
 *         | Some c -> c
 *         | None   -> raise (Invalid_request req)
 *       in
 *       match square_with_amazon_of_color
 *       let coord = param req "coord" |> Json.To.coord in
 *       Page.make [H.p [H.pcdata @@
 *                       Printf.sprintf
 *                         "x is: %i and y is: %i"
 *                         coord.x coord.y]]
 *       |> respond'
 *     end *)

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
  (* |> select_amazon *)
  |> App.run_command
