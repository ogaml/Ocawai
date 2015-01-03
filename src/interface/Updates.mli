(** Module to handle updates in the interface *)

(** Type representing the turn of a player *)
type turn =
  | Your_turn
  | Turn_of of int
  | Nobody_s_turn

(** Class that represents update handlers *)
class handler : ClientData.client_data -> Camera.camera -> object

  (** Method to be called at each frame to update *)
  method update : unit

  (** @return the position and offset required to print a given unit *)
  method unit_position : Unit.t -> (Position.t * (float * float))

  (** @return the player whose turn is the current *)
  method current_turn : turn

end