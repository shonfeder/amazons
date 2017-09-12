module Param = Eliom_parameter

let default_css = [["css"; "amazons.css"]]
let current_service = Eliom_service.reload_action

module State = struct
  type game_id = int

  let next_game_number : game_id ref
    = ref 0
  let games : (game_id * Game.t) list ref
    = ref []
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
  let link ?attr service text =
    let open Eliom_content.Html.D in
    a ~service [pcdata text]

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

  let new_game = Make.service
      ~path:["games"; "new"]
      ~meth:(Get Param.unit)

  let game = Make.service
      ~path:["games"; ""]
      ~meth:(Get Param.(suffix @@ int "game_id"))

  let games = Make.service
      ~path:["games"; ""]
      ~meth:(Get Param.unit)
end

module Content = struct
  open Eliom_content.Html.D

  module Page = struct
    (* TODO Correct documentation on basic menus
       - correct this http://ocsigen.org/eliom/6.2/manual/misc#basic_menu
       - with reference to this https://github.com/ocsigen/eliom/blob/master/src/lib/eliom_tools.eliom#L212
       TODO Remove current, since it is the default *)
    let menu =
      let items =
        [ (Service.home,  [pcdata "Home"])
        ; (Service.games, [pcdata "Games"]) ]
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

  let games =
    Page.body
      [ h2 [pcdata "Ongoing Games"]
      ; p  [pcdata "A list of games should be here"] ]

  let game game_id =
    Page.body
      [ h1 [pcdata ("Game " ^ string_of_int game_id ^ " should be here!")]
      ; Render.Html.board Game.Board.empty ]

  let new_game =
    Page.body
      [ h2 [pcdata "Create a New Game"]
      ; p  [pcdata "Interface to create a new game will be here"] ]
end

(* TODO Functorize?

   module type EndPoint =
     type t
     val registrar : Eliom_registration.('get -> 'post -> page Lwt.t) -> unit
     val service   : Eliom_service.t
     val content   : Eliom_content.Html.elt
   end

   module Make (E:EndPoint) = struct
     val end_point = E.registrar E.Service E.content
   end

   Then we can define each endpoint in its own module.

   We can specify a list of these modules, and just iterate the functor over
   them.

   Is this actually better?
*)

let amazons_service =
  Register.html Service.home
    (Make.page
       ~title:"The Game of the Amazons"
       ~body:Content.home)

let new_game_service =
  Register.html Service.new_game
    (Make.page
       ~title:"A New Game of the Amazons"
       ~body:Content.new_game)

(* TODO If an existing game does not match id, give 404ish *)
let game_service =
  Register.html Service.game
    (Make.page_param
       ~title:"An Ongoing Game of the Amazons"
       ~body:Content.game)

let games_service =
  Register.html Service.games
    (Make.page
       ~title:"Games of the Amazons"
       ~body:Content.games)
