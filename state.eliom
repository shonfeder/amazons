[%%server.start]

let lookup = CCList.Assoc.get

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
              ; games := (!game_number, Game.Update.start) :: !games
              ; !game_number)

(* TODO Implement using
   https://ocsigen.org/eliom/6.2/api/server/Eliom_cscache *)
let game id =
  lookup id (!games)
