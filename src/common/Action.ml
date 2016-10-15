(** Action functions *)

type movement = Position.t list

type action =
| Attack_unit of (Unit.unit_id * Unit.unit_id)
| Create_unit of (Building.building_id * Unit.unbound_t)
| Wait
| End_turn
| None_

type t = movement * action


 (* TODO: implement this method *)
let from_string (str : string) =
  [Position.create(0,0)],Wait

 (* TODO: implement this method *)
let to_string (a : t) =
  ""


let mov_to_string (m : movement) =
  ""
  
let mov_from_string (str : string) =
  Position.create(0,0)::[]


exception Bad_unit
exception Has_played
exception Bad_path
exception Bad_attack
exception Bad_create
