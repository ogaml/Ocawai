(** Mixins: widget bases / properties for the GUI to mix by inheritance *)

open OgamlGraphics

(** A mixin that represents widget containers *)
class virtual ['a] widget_container : object

  inherit Widget.widget

  constraint 'a = #Widget.widget

  method add_child : 'a -> unit

  method children : 'a list

  (** Removes all children *)
  method clear_children : unit

end


(** A mixin that represents widgets with a theme associated to them *)
class virtual themed_widget : object

  val virtual mutable theme : Theme.t

end


(** A mixin that represents an Expandable Vertical container, where all
  * item heights are equal *)

class virtual ['a] evq_container : object

  inherit ['a] widget_container

  val virtual mutable item_height : int

end


(** A mixin that represents a keyboard-controlled list
  * The two parameters are the up/down keys *)
class virtual key_ctrl_list : OgamlCore.Keycode.t ->
  OgamlCore.Keycode.t -> object

  val virtual mutable nb_items : int

  method virtual add_event : (OgamlCore.Event.t -> bool) -> unit

  method selected : int

  method reset_selection : unit

end


(** A mixin that represents widgets with a toolbar *)
class virtual has_toolbar : object

  inherit Widget.widget

  inherit themed_widget

  val virtual mutable toolbar_height : int

  val virtual mutable toolbar_icon : string

  val virtual mutable toolbar_text : string

  method draw : Window.t -> TextureLibrary.t -> unit

end
