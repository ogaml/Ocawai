(** About the minimap *)

(** Class representing a minimap *)
class minimap : int -> int -> int -> object

  (** Updates informations on the minimap *)
  method compute : Battlefield.t -> Player.logicPlayer list -> unit

  (** Draws the minimap *)
  method draw : (module OgamlGraphics.RenderTarget.T with type t = 'a) -> 'a ->
                Cursor.cursor -> unit

end
