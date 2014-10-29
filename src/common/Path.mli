(** Made to handle paths, their creation. *)

(** Hidden type of paths for this module.
  * It is different from the one described in [Action]. *)
type t

(** @return an empty path. *)
val empty : t

(** [init pos] returns a path starting at [pos]. *)
val init : Position.t -> t

(** [reach path pos] returns the path composed of [path] followed by [pos].
  * Later it might even compute a new path, given [path] that has the same
  * starting position and ending in [pos].
  * It takes care of validity (or so it should). *)
val reach : t -> Position.t -> t

(** @return an [Action.movement] representing the same path. *)
val get_move : t -> Action.movement

(** @ operator on paths *)
val cat : t -> t -> t

(** @return the movement cost of a path, not counting the initial position.
    The path must be possible for the given movement type. *)
val cost : Unit.movement -> Battlefield.t -> t -> int

(** [dijkstra m pos1 Unit.Walk pos2] return [Some cost * path] for going to pos2 from pos1, or [None] if pos2 is not reachable from pos1 *)
val dijkstra : Battlefield.t -> Position.t -> Unit.movement -> Position.t -> ( int * t ) option