open Opium.Std
module T = Tyxml
module H = T.Html

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
end

let home = get "/"
    begin fun req ->
      let open T.Html in
      Page.make [h1 [pcdata "The Game of the Amazons"]]
      |> respond'
    end

let game = get "/game"
    begin fun req ->
      [Render.Html.game Game.Update.start]
      |> Page.make
      |> respond'
    end

let move = get "/game/move/:coord"
    begin fun req ->
      let coord = param req "coord" |> Json.To.coord in
      Page.make [H.p [H.pcdata @@
                      Printf.sprintf
                        "x is: %i and y is: %i"
                        coord.x coord.y]]
      |> respond'
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
  |> move
  |> App.run_command
