(** Rendering module *)

(** The renderer in itself *)
val renderer : <

  (** Draw a tile from a tileset *)
  draw_direct_tile : OgamlGraphics.Window.t -> Tileset.tileset ->
    string ->
    ?position:(float * float) ->
    ?rotation:float ->
    ?scale:(float*float) ->
    ?color:OgamlGraphics.Color.t ->
    ?origin:(float*float) -> unit -> unit;

  (** Draw a [texture] in screen coordinates (from the local library).
    * Usage: [draw_txr target name ... ]*)
  draw_txr : OgamlGraphics.Window.t -> string ->
    ?position:(float * float) ->
    ?rotation:float ->
    ?scale:(float*float) ->
    ?size:(float*float) ->
    ?color:OgamlGraphics.Color.t ->
    ?centered:bool ->
    ?blend_mode:OgamlGraphics.DrawParameter.BlendMode.t -> unit -> unit;

  (** TODO Draw the GUI *)
(*   draw_gui : OgamlGraphics.Window.t -> *)
(*     UIManager.ui_manager -> unit; *)

  (** Draw the whole game on the screen *)
  render_game : OgamlGraphics.Window.t ->
    ClientData.client_data -> Updates.handler -> unit;

  (** Load the various ressources stored in ressources/ *)
  init : OgamlGraphics.Window.t -> unit

>
