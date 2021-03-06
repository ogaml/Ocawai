(** Tile operations *)

(** tile type*)
type t

(** type of tiles for generation *)
type structure = [ `Block | `Road | `Border of (string * int * int) ]

(** [get_name tile] returns the name of the tile *)
val get_name : t -> string

(** [get_density tile] returns the generation factor used by FieldGenerator *)
val get_density : t -> int

(** [get_frow_speed tile] returns the expantion speed of [tile] used by the generation by seeds *)
val get_grow_speed : t -> int

(** [get_structure tile] returns the structure type *)
val get_structure : t -> structure

(** Check if a tile is traversable by a given type of movement *)
val traversable_m : t -> Unit.movement -> bool

(** Check if a tile is traversable by a given unit *)
val traversable : t -> Unit.t -> bool

(** Compare two tiles in term of movements.
  [compare_movements t1 t2] is: 
    Some i <=0 if t1 possible movements are included in t2 possible movements
    Some i >=0 if t2 possible movements are included in t1 possible movements
    None if their possiblities are not comparable *)
val compare_movements : t -> t -> int option

(** Compare two tiles in term of Walk, Roll and Tread movements.
  [compare_walkability t1 t2] is: 
    Some i <=0 if t1 ground movements are included in t2 ground movements
    Some i >=0 if t2 ground movements are included in t1 ground movements
    None if their possiblities are not comparable *)
val compare_walkability : t -> t -> int option

(** Take a movement type and return a tile cost. *)
val movement_cost : t -> Unit.movement -> int

(** Take a unit and return a tile cost. *)
val tile_cost : t -> Unit.t -> int

(** Create a tile from a parsed record*)
val parsed_tile_to_tile : Tile_t.t -> t

(** Create a parsed record from a tile*)
val tile_to_parsed_tile : t -> Tile_t.t

