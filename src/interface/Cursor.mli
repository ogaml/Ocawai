class cursor : position:Position.t -> object

  method set_position : Position.t -> unit

  method position : Position.t

  method set_moving : unit

  method stop_moving : unit

  method toggle_moving :  unit

  method get_move : Action.movement

  method scale : float

end