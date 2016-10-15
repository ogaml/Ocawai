(** Module for menus *)

(** A class representing an item (in a menu) *)
class item : ?enabled:bool -> string -> string -> (unit -> unit) -> object

  inherit Widget.widget

  val mutable position : int * int

  val mutable size : int * int

  method draw : OgamlGraphics.Window.t -> TextureLibrary.t -> unit

  method action : unit

end

(** A class representing key-controlled buttons *)
class key_button : icon:string -> text:string -> m_position:(int*int) ->
  m_size:(int*int) -> keycode:OgamlCore.Keycode.t ->
  callback:(unit -> unit) -> m_theme:Theme.t -> object

  inherit Widget.widget

  val mutable position : int * int

  val mutable size : int * int

  method draw : OgamlGraphics.Window.t -> TextureLibrary.t -> unit

  method set_callback : (unit -> unit) -> unit

end


(** Usage : [new ingame_menu position width item_height theme bar_height
  * bar_icon bar_text]
  * Creates a simple ingame menu *)
class ingame_menu :
  ?escape:(unit -> unit) ->
  m_position:(int * int) ->
  m_width:int ->
  m_item_height:int ->
  m_theme:Theme.t ->
  m_bar_height:int ->
  m_bar_icon:string ->
  m_bar_text:string ->
  unit ->
  object

  inherit [item] BaseMixins.evq_container

  inherit BaseMixins.key_ctrl_list

  val mutable position : int * int

  val mutable size : int * int

  val mutable nb_items : int

  val mutable item_height : int

  val mutable escape : (unit -> unit) option

  method set_escape : (unit -> unit) -> unit

  method draw : OgamlGraphics.Window.t -> TextureLibrary.t -> unit

end
