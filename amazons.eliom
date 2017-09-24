module Param = Eliom_parameter

let default_css = [["css"; "amazons.css"]]
let current_service = Eliom_service.reload_action

module State = struct

  type game_id = int
  let game_number
    : game_id ref
    = ref 0
  let games
    : (game_id * Game.t) list ref
    = ref []

  (** [new_game] increments the [game_number] counter, starts a new
      game associated with the new [game_number] in [games], and
      returns the new [game_number]. *)
  let new_game
    : unit   -> game_id
    = fun () -> ( game_number := !game_number + 1
                ; games := (!game_number, Game.start) :: !games
                ; !game_number)

end

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
  let link service text =
    let open Eliom_content.Html.D in
    a ~service [pcdata text] ()

  let page ~title ?(style=default_css) ~body =
    fun () () -> Lwt.return @@
      Eliom_tools.F.html
        ~title:title
        ~css:style
        (body ())

  let page_param ~title ?(style=default_css) ~body =
    fun param () -> Lwt.return @@
      Eliom_tools.F.html
        ~title:title
        ~css:style
        (body param ())

  let service ~path ~meth =
    Eliom_service.(create ~path:(Path path) ~meth ())
end

module Service = struct
  open Eliom_service

  let home = Make.service
      ~path:[""]
      ~meth:(Get Param.unit)

  let games = Make.service
      ~path:["games"; ""]
      ~meth:(Get Param.unit)

  let game =
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
          (html
             (head (title (pcdata "Temp game page")) [])
             (body
                [ h2 [pcdata page_title]
                ; content ]))

  let new_game = let open Eliom_service in
    Eliom_registration.Redirection.create
      ~options:`TemporaryRedirect
      ~path:(Path ["games"; "new"])
      ~meth:(Get Param.unit)
      (fun () () ->
         let game_id = State.new_game () in
         Lwt.return @@
         Eliom_registration.Redirection
           (preapply game game_id))
end

module Content = struct
  open Eliom_content.Html.D

  module Page = struct
    let menu =
      let items =
        [ (Service.home,     [pcdata "Home"])
        ; (Service.games,    [pcdata "Games"])
        ; (Service.new_game, [pcdata "New Game"])]
      in
      Eliom_tools.D.menu
        ~classe:["main-menu"]
        items
        ~service:Eliom_service.reload_action

    let header () =
      [div [ h1 [pcdata "The Game of the Amazons"]
           ; menu () ]]

    let footer =
      [div [ p [pcdata "Footer content here"] ]]

    let body content () =
      body (header () @ content @ footer)
  end

  let home =
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
  let games games =
    let game_item (game_id, _) =
      let service = Eliom_service.preapply Service.game game_id
      and text    = "Game " ^ (string_of_int game_id)
      in
      li [Make.link service text]
    in
    let game_items = List.map game_item games
    in
    Page.body
      [ h2 [pcdata "Ongoing Games"]
      ; ul game_items ]

  let game game_id =
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
    Page.body
      [ h2 [pcdata "Create a New Game"]
      ; p  [pcdata "Interface to create a new game will be here"] ]
end

let amazons_service =
  Register.html Service.home
    (Make.page
       ~title:"The Game of the Amazons"
       ~body:Content.home)

let games_service =
  Register.html Service.games
    (fun () () ->
       let games = List.rev (!State.games) in
       Lwt.return @@ Eliom_tools.F.html
         ~title:"Games of the Amazons"
         ~css:default_css
         (Content.games games ()))

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
