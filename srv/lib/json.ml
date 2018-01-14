open Core
open Game

module Of = struct
  let coord Coord.{x; y} =
    let open Ezjsonm in
    dict [ "x", int x
         ; "y", int y ]
end

module To = struct
  let coord json =
    let open Ezjsonm in
    let get_axis axis j = find j [axis] |> get_int in
    let data = json
               |> from_string
               |> value
    in
    let x = get_axis "x" data
    and y = get_axis "y" data
    in
    Coord.{x; y}
end
