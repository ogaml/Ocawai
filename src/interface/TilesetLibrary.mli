(** Handles textures *)

type t

exception Unknown_tileset of string

val create : unit -> t

(** Recursively load the textures of a directory and its sub-directories *)
val load_directory : OgamlGraphics.Window.t -> t -> string -> unit

val load_tileset : OgamlGraphics.Window.t -> t -> string -> unit

val get_tileset : t -> string -> Tileset.tileset
