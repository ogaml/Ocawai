(** Home screen and menus (before game) *)

(* TODO inherit from BaseMixins *)

(** An item for the screen
  * It has a [position] that represents its center *)
class virtual item : object

  method virtual draw : OgamlGraphics.Window.t -> unit

  method virtual position : float * float
  method x : float
  method y : float

  method holds_focus : bool
  method handle_key : OgamlCore.Keycode.t -> unit

end

(** An actionnable item *)
class virtual actionnable : object

  inherit item

  val mutable has_focus : bool

  method set_focus : bool -> unit

  method virtual action : unit

end

(** A modal item
  * It retains the focus when activated *)
class virtual modal : object

  inherit actionnable

  val mutable holds_focus : bool

  method holds_focus : bool
  method virtual handle_key : OcsfmlWindow.KeyCode.t -> unit

end

(** A screen item
  * [position] should be given wrt to the center of the texture *)
class textured_item : string -> (float * float) -> object

  inherit item

  method draw : OgamlGraphics.Window.t -> unit

  method position : float * float

end

(** An actionnable item *)
class textured_actionnable : string -> string -> (float*float) -> (unit -> unit) ->
object

  inherit textured_item
  inherit actionnable

  method action : unit

  method draw : OgamlGraphics.Window.t -> unit

end

(** A screen (simply a menu)
  * It has the particularity it handles selection with position.
  * TODO: Handle intro; outro; multiple textures and interpolators *)
class screen : item list -> actionnable list -> object

  method draw : OgamlGraphics.Window.t -> unit

  method handle_key : OgamlCore.Keycode.t -> unit

end
