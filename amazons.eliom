module Param = Eliom_parameter

let default_css = [["css"; "amazons.css"]]
let current_service = Eliom_service.reload_action

module Amazons = struct
  module Info = struct
    let application_name = "amazons"
    let global_data_path = None
  end
  module App = Eliom_registration.App (Info)
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
  let link service text =
    let open Eliom_content.Html.D in
    a ~service [pcdata text] ()

  let service ~path ~meth =
    Eliom_service.(create ~path:(Path path) ~meth ())
end

module Content = struct
  module Html = Eliom_content.Html.D

  module Page = struct

    let site_menu items : unit -> [> `Ul ] Eliom_content.Html.elt =
      Eliom_tools.D.menu
        ~classe:["main-menu"]
        items
        ~service:Eliom_service.reload_action

    let header menu_items =
      let open Html in
      div [ h1 [pcdata "The Game of the Amazons"]
          ; site_menu menu_items ()
          ]

    let footer =
      let open Html in
      div [ p [pcdata "Footer content here"] ]

    let body content menu_items =
      Html.body ([header menu_items] @ content @ [footer])
  end

  let home menu_items =
    let open Html in
    Page.body
      [ h2 [pcdata "Rules"]
      ; p  [ Make.External.link
               ~domain:"http://wikipedia.org"
               ~path:["wiki"; "Game_of_the_Amazons"]
               "The Game of the Amazons on Wikipedia"
           ; br ()
           ]
      ]
      menu_items

  (* TODO refactor this bit out into a "game room"
     The game room will contain the context of play
     and will also be run through the renderer.contents

     It can take care of things like the interface for pairing players
     and for the "tables" where play happens, etc. *)
  let games game_service games =
    let open Html in
    let game_item (game_id, _) =
      let service = Eliom_service.preapply game_service game_id
      and text    = "Game " ^ (string_of_int game_id)
      in
      li [Make.link service text]
    in
    let game_items = List.map game_item games in
    Page.body
      [ h2 [pcdata "Ongoing Games"]
      ; ul game_items ]

  let game game_id =
    let open Html in
    let lookup = BatList.Exceptionless.assoc in
    let (title, content) =
      match lookup game_id (!State.games) with
      | None ->
        ("Sorry!",
         p [pcdata "This game doesn't exist"])
      | Some game ->
        ("Game of the amazons number " ^ string_of_int game_id,
         Render.Html.game game)
    in
    Page.body
      [ h2 [pcdata title]
      ; content ]

end

module Service = struct
  open Eliom_service

  let rec home =
    Make.service
      ~path:[""]
      ~meth:(Get Param.unit)
  and home_registration () =
    Eliom_registration.Html.register
      home
      (fun () () ->
         Lwt.return @@
         Eliom_tools.F.html
           ~title:"The Game of the Amazons"
           ~css:default_css
           (Content.home @@ services_menu_items ()))

  and game_app () =
    Amazons.App.create
      ~path:(Path ["games"; ""])
      ~meth:(Get Param.(suffix @@ int "game_id"))
      (fun game_id () ->
         Lwt.return @@
         Eliom_tools.D.html
           ~title:"A Game of the Amazons"
           ~css:default_css
           (Content.game game_id @@ services_menu_items ()))

  and new_game_redirection () = let open Eliom_service in
    Eliom_registration.Redirection.create
      ~options:`TemporaryRedirect
      ~path:(Path ["games"; "new"])
      ~meth:(Get Param.unit)
      (fun () () ->
         let game_id = State.new_game () in
         Lwt.return @@
         Eliom_registration.Redirection
           (preapply (game_app ()) game_id))

  and games  =
    Make.service
      ~path:["games"; ""]
      ~meth:(Get Param.unit)
  and games_registration () =
    Eliom_registration.Html.register
      games
      (fun () () ->
         let games = List.rev (!State.games) in
         Lwt.return @@ Eliom_tools.F.html
           ~title:"Games of the Amazons"
           ~css:default_css
           (Content.games (game_app ()) games (services_menu_items ())))

  and services_menu_items () =
    let open Eliom_content.Html.D in
    [ (home,                    [pcdata "Home"])
    ; (games,                   [pcdata "Games"])
    ; (new_game_redirection (), [pcdata "New Game"])]

  let register_services =
    games_registration () ;
    home_registration ()
    (* new_game is registered in the menu *)

end
