(** @author Paul-Gallot Julien Grange et François Thiré *)

(** Error that can be sent by the engine if a client use a wrong id*)
type error = Wrong_id_player


type id_player = int

(** Type of the data sent from the engine to the player/client *)
type update =
    Game_over
  | Your_turn
  | Classement 
  | Set_army of Unit.t list * id_player
  | Set_building of Building.t list * id_player
  | Add_unit of Unit.t * id_player
  | Add_building of Building.t * id_player
  | Delete_unit of Unit.unit_id *id_player (*Fog or kill*)
  | Delete_building of Building.building_id * id_player(*fog or kill*)
  | Move_unit of Unit.unit_id * Action.movement * id_player
  | Set_unit_hp of Unit.unit_id * int * id_player
(* for initialization only *)
  | Set_client_player of id_player
  | Set_logic_player_list of id_player list
  | Map of string
  | Building_changed of Building.t

                       

(**what a NetPlayer send to a client player*)
type  send = Get_next_action | Update of update

(**What a client player send to a Net Player *)
type  receive = Next_action of Action.t | Error of error

