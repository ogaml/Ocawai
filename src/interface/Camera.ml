open Utils
open Settings_interface_t

class camera ~def_tile_size ~w ~h ~initpos ~maxpos = object(self)

  val cursor = new Cursor.cursor ~position:initpos

  val mutable offset = (0., 0.)

  val mutable zoom_factor = 1.

  val mutable min_zoom =
    let (a,b) = Position.topair maxpos in
    let w = float_of_int w in
    let h = float_of_int h in
    min (w /. ((float_of_int (def_tile_size * a)) *. 1.5))
        (h /. ((float_of_int (def_tile_size * b)) *. 1.5))

  val mutable max_zoom = 2.5

  method cursor = cursor


  (* Display functions *)
  method project p =
    let offset_i = iof2D offset in
    let (x,y) = Position.topair (Position.diff p cursor#position) in
    let (dx,dy) = (x * self#tile_size + (fst offset_i),
                   y * self#tile_size + (snd offset_i)) in
    (dx + w/2, dy + h/2)

  method top_left =
    let offset_i = iof2D offset in
    let p = Position.create
      ((w + (fst offset_i * 2))/(2*self#tile_size) + 1,
       (h + (snd offset_i * 2))/(2*self#tile_size) + 1) in
    Position.clamp
      (Position.diff cursor#position p)
      (Position.create (0,0))
      maxpos

  method bottom_right =
    let offset_i = iof2D offset in
    let p = Position.create
      ((w - (fst offset_i * 2))/(2*self#tile_size) + 1,
       (h - (snd offset_i * 2))/(2*self#tile_size) + 1) in
    Position.clamp
      (Position.add cursor#position p)
      (Position.create (0,0))
      maxpos

  method tile_size = int_of_float (float_of_int def_tile_size *. self#zoom)


  (* Move functions *)
  method move displ =
    let new_position = Position.clamp
      (Position.add (Position.create displ) cursor#position)
      (Position.create (0,0))
      maxpos
    in
    let old_cursor = cursor#position in
    let new_cursor = cursor#set_position new_position in
    let cursor_speed = Config.config#settings_interface.cursor_speed in
    let (dx, dy) = Position.topair
      (Position.diff new_cursor old_cursor)
    in
    (* Interpolating camera *)
    let (offx, offy) = foi2D (dx * self#tile_size, dy * self#tile_size) in
    offset <- addf2D offset (offx, offy);
    let interp_function t dt =
      offset <- addf2D offset (-. dt *. cursor_speed *. offx /. 5.,
                               -. dt *. cursor_speed *. offy /. 5.)
    in
    ignore(Interpolators.new_ip_with_timeout interp_function (5./.cursor_speed));
    (* Interpolating cursor *)
    let (offx, offy) = foi2D (def_tile_size * dx, dy * def_tile_size) in
    cursor#set_offset (addf2D cursor#offset (offx, offy));
    let interp_cursor t dt =
      cursor#set_offset (addf2D cursor#offset
        (-. dt *. cursor_speed *. offx,
         -. dt *. cursor_speed *. offy))
    in
    ignore(Interpolators.new_ip_with_timeout interp_cursor (1./.cursor_speed))
    

  method set_position pos =
    let clamped = Position.clamp pos (Position.create (0,0)) maxpos in
    self#move (Position.topair (Position.diff clamped cursor#position))


  (* Zoom functions *)
  val mutable zoom_target = 1.

  method zoom = zoom_factor

  method set_zoom z =
    let zoom_speed = Config.config#settings_interface.zoom_speed in
    let end_zoom = min (max z min_zoom) max_zoom in
    let begin_zoom = zoom_target in
    zoom_target <- end_zoom;
    let interp_function t dt =
      zoom_factor <- zoom_factor +. (end_zoom -. begin_zoom) *. zoom_speed *. dt
    in
    ignore(
      Interpolators.new_ip_with_timeout interp_function (1./.zoom_speed))

  method toggle_zoom =
    if zoom_factor > (min_zoom *. 1.5) then self#set_zoom min_zoom
    else self#set_zoom 1.

end
