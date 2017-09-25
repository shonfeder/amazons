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

  let page ~title ?(style=default_css) ~body menu_items =
    fun () () -> Lwt.return @@
      Eliom_tools.F.html
        ~title:title
        ~css:style
        (body menu_items)

  let page_param ~title ?(style=default_css) ~body menu_items=
    fun param () -> Lwt.return @@
      Eliom_tools.F.html
        ~title:title
        ~css:style
        (body param menu_items)

  let service ~path ~meth =
    Eliom_service.(create ~path:(Path path) ~meth ())
end

module Content = struct
  module Html = Eliom_content.Html.D

  module Page = struct

    let menu items : unit -> [> `Ul ] Eliom_content.Html.elt =
      Eliom_tools.D.menu
        ~classe:["main-menu"]
        items
        ~service:Eliom_service.reload_action

    let header menu_items =
      let open Html in
      [div [ h1 [pcdata "The Game of the Amazons"]
           (* ; menu menu_items () *)
           ]]

    let footer =
      let open Html in
      [div [ p [pcdata "Footer content here"] ]]

    let body content menu_items =
      Html.body (header menu_items @ content @ footer)
  end

  let home =
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

  let new_game =
    let open Eliom_content.Html.D in
    Page.body
      [ h2 [pcdata "Create a New Game"]
      ; p  [pcdata "Interface to create a new game will be here"] ]
end

module Service = struct
  open Eliom_service

  let rec home =
    Make.service
      ~path:[""]
      ~meth:(Get Param.unit)
  and home_service =
    Eliom_registration.Html.register
      home
      (Make.page
         ~title:"The Game of the Amazons"
         ~body:Content.home services_menu)

  and game =
    Amazons.App.create
      ~path:(Path ["games"; ""])
      ~meth:(Get Param.(suffix @@ int "game_id"))
      @@ fun game_id () ->
        let lookup = BatList.Exceptionless.assoc in
        let open Eliom_content.Html.D in
        let (page_title, content) =
          match lookup game_id (!State.games) with
          | None ->
            ("Sorry!",
             p [pcdata "This game doesn't exist"])
          | Some game ->
            ("Game of the amazons number " ^ string_of_int game_id,
             Render.Html.game game)
        in
        Lwt.return
          (* Make.page *)
          (html
             (head (title (pcdata "Temp game page")) [])
             (body
                [ h2 [pcdata page_title]
                ; content ]))

  and new_game = let open Eliom_service in
    Eliom_registration.Redirection.create
      ~options:`TemporaryRedirect
      ~path:(Path ["games"; "new"])
      ~meth:(Get Param.unit)
      (fun () () ->
         let game_id = State.new_game () in
         Lwt.return @@
         Eliom_registration.Redirection
           (preapply game game_id))

  and games  =
      Make.service
      ~path:["games"; ""]
      ~meth:(Get Param.unit)
  and games_service =
    Eliom_registration.Html.register
      games
      (fun () () ->
         let games = List.rev (!State.games) in
         Lwt.return @@ Eliom_tools.F.html
           ~title:"Games of the Amazons"
           ~css:default_css
           (Content.games game games services_menu))

  and services_menu =
    let open Eliom_content.Html.D in
    [ (home,     [pcdata "Home"])
    ; (games,    [pcdata "Games"])
    ; (new_game, [pcdata "New Game"])]

end


(******************)
(* Example of redirecting to an application *)

module Test = struct
  module Info = struct
    let application_name = "Test"
    let global_data_path = None
  end
  module App = Eliom_registration.App (Info)
end

let test_service =
  let open Eliom_service in
  Test.App.create
    ~path:(Path ["test"])
    ~meth:(Get Eliom_parameter.(suffix (int "test_param")))
    (fun test_param () ->
       let open Eliom_content.Html.D in
       Lwt.return
         (html
            (head (title (pcdata "Test")) [])
            (body [ h1 [pcdata "Test"]
                  ; p  [pcdata (string_of_int test_param)]]) ))

let redirect_test =
  let open Eliom_service in
  Eliom_registration.Redirection.create
    ~options:`TemporaryRedirect
    ~path:(Path ["redirect"])
    ~meth:(Get Param.unit)
    @@ fun () () ->
    let () = print_string "Test side effect" in
    Lwt.return @@
    Eliom_registration.Redirection
      (preapply test_service 1)
