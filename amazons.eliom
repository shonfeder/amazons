module Param = Eliom_parameter

let default_css = [["css"; "amazons.css"]]

[%%shared
  type game_id = int ref

  let game_ids : game_id = ref 0
  let active_games : game_id list = []
]

module Amazons = struct
  module Info = struct
    let application_name = "amazons"
    let global_data_path = None
  end
  module App = Eliom_registration.App (Info)
end

module Register = struct
  open Eliom_registration
  let html = Html.register
end

(** Convenience functions to hide the complexity of Eliom stuff *)
module Make = struct

  module External = struct
    let link ?(domain="") ?(path=[]) text =
      let open Eliom_content.Html.D in
      a
        Eliom_service.(
        extern
          ~prefix:domain
          ~path:path
          ~meth:(Get Eliom_parameter.unit)
          ())
        [pcdata text]
        ()
  end

  (* TODO Add attribute handling? *)
  let link ?attr service text =
    let open Eliom_content.Html.D in
    a ~service [pcdata text]

  let page ~title ?(style=default_css) ~body =
    fun () () -> Lwt.return @@
      Eliom_tools.F.html
        ~title:title
        ~css:style
        body

  let service ~path ~meth =
    Eliom_service.(create ~path:(Path path) ~meth ())
end

module Service = struct
  open Eliom_service

  let home = Make.service
      ~path:[""]
      ~meth:(Get Param.unit)

  let game = Make.service
      ~path:["games"; ""]
      ~meth:(Get Param.(suffix @@ int "game_id"))

  let games = Make.service
      ~path:["games"; ""]
      ~meth:(Get Param.unit)

  let current = reload_action
end

module Content = struct
  open Eliom_content.Html.D

  module Page = struct
    (* TODO pass current service in to menu (try it at least)*)
    (* TODO Correct documentation on basic menus
       - correct this http://ocsigen.org/eliom/6.2/manual/misc#basic_menu
       - with reference to this https://github.com/ocsigen/eliom/blob/master/src/lib/eliom_tools.eliom#L212 *)
    let menu current = Eliom_tools.D.menu
        ~classe:["main-menu"]
        [ (Service.home, [pcdata "testing"])
        ; (Service.games, [pcdata "games"]) ]
        ~service:current
        ()

    let nav_bar = ul ~a:[a_class ["nav-bar"]]
                     [ li [pcdata "Link to Home"]
                     ; li [pcdata "Link to Games"]
                     ; li [pcdata "Link to About"]]
    let header =
      [div [ h1  [pcdata "The Game of the Amazons"]
           ; nav_bar ]]
    let footer =
      [div [ p [pcdata "Footer content here"] ]]
    let body content =
      body (header @ content @ footer)
  end

  let new_game_button =
    let onclick_handler =
      [%client
      (fun _ ->
         let next_game_id = ~%game_ids := !(~%game_ids) + 1; !(~%game_ids)
         in
         let game_id = string_of_int next_game_id
         in
         Dom_html.window##alert(Js.string ("Input value :" ^ game_id)))
      ]
    in
    button ~a:[a_onclick onclick_handler] [pcdata "Create a new game"]

  let home =
    Page.body
      [ h2 [pcdata "Rules"]
      ; p  [ Make.External.link
               ~domain:"http://wikipedia.org"
               ~path:["wiki"; "Game_of_the_Amazons"]
               "The Game of the Amazons on Wikipedia"
           ; br ()
           ; new_game_button ]
      ]

  let games =
    Page.body
      [ h2 [pcdata "Ongoing Games"]
      ; p  [pcdata "A list of games should be here"] ]

  let game game_id =
    Page.body
      [ h1 [pcdata ("Game " ^ string_of_int game_id ^ " should be here!")]
      ; Render.Html.board Game.Board.empty ]
end

let amazons_service =
  Register.html Service.home
    (Make.page
       ~title:"The Game of the Amazons"
       ~body:Content.home)

let games_service = let open Eliom_service in
  Register.html Service.games
    (Make.page
       ~title:"Games of the Amazons"
       ~body:Content.games)

(* TODO If an existing game does not match id, give 404ish *)
let game_service = let open Eliom_service in
  Register.html Service.game
    begin
      fun game_id () -> Lwt.return @@
        Eliom_tools.F.html
          ~title:"A Game of the Amazons"
          ~css:default_css
          (Content.game game_id)
    end
