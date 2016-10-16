open OgamlGraphics
open GuiTools
open Widget
open BaseMixins
open Utils

let my_font = Font.load "resources/fonts/FreeSans.ttf"

class item ?enabled:(enabled = true) icon text (action : unit -> unit) =
  object(self)

  inherit widget

  (* Undefined until added to a container *)
  val mutable position = (0,0)

  val mutable size = (0,0)

  method draw target lib = if self#active then begin
    (* First draw the icon *)
    let color = Color.(if enabled
      then `RGB RGB.white
      else `RGB RGB.({ r = 0.6 ; g = 0.6 ; b = 0.6 ; a = 1. }))
    in
    let position = foi2D self#position in
    let (selfx, selfy) = foi2D size in
    let position,size = OgamlMath.Vector2f.(
      (let (x,y) = position in { x ; y }),
      { x = selfy ; y = selfy }
    ) in
    Render.renderer#draw_txr
      target icon ~position ~size ~centered:false ~color ();
    (* Then draw the text *)
    let x,y,width,height = OgamlMath.Vector2f.(
      position.x +. selfy, position.y, selfx -. selfy, selfy
    ) in
    rect_print (module Window)
      target text my_font
        (if enabled then (Color.(`RGB RGB.black))
         else Color.(`RGB RGB.({ r = 0. ; g = 0. ; b = 0. ; a = 0.5 })))
        (Pix (int_of_float selfy - 3)) (Pix 2) Left
        OgamlMath.FloatRect.({ x ; y ; width ; height })
  end

  method action = if enabled then action ()

end


class key_button ~icon ~text ~m_position ~m_size ~keycode
  ~callback ~m_theme = object(self)

  inherit widget

  inherit themed_widget

  val mutable position = m_position

  val mutable size = m_size

  val mutable theme = m_theme

  val mutable callback : unit -> unit = callback

  initializer
    self#add_event OgamlCore.(function
      | Event.KeyPressed { Event.KeyEvent.key = kc ; _ } when keycode = kc ->
          (callback (); true)
      | _ -> false)

  method set_callback c = callback <- c

  method draw target lib = if self#active then begin
    let shape =
      let position, size = OgamlMath.Vector2f.(
        (let (x,y) = foi2D self#position in {x;y}),
        (let (x,y) = foi2D size in {x;y})
      ) in
      Shape.create_rectangle
        ~position ~size ~color:theme.Theme.default_color
        ~border_color:theme.Theme.border_color ~thickness:2. ()
    in
    Shape.draw (module Window) target shape () ;

    let (selfx, selfy) = foi2D size in
    let position = foi2D self#position in
    let vpos = let (x,y) = position in OgamlMath.Vector2f.({x;y}) in
    let vsize = OgamlMath.Vector2f.({ x = selfy ; y = selfy }) in
    Render.renderer#draw_txr
      target icon ~position:vpos ~size:vsize ~centered:false () ;

    let x,y = position in
    rect_print (module Window)
      target text my_font (Color.(`RGB RGB.black)) (Pix (snd size - 1)) (Pix 2)
      Center OgamlMath.FloatRect.({
        x = x +. selfy ;
        y = y ;
        width = selfx -. selfy ;
        height = selfy })
  end

end


class ingame_menu ?escape ~m_position ~m_width ~m_item_height ~m_theme ~m_bar_height
  ~m_bar_icon ~m_bar_text ()
  = object(self)

  inherit [item] evq_container as super

  inherit key_ctrl_list OgamlCore.Keycode.Up OgamlCore.Keycode.Down as kcl

  inherit has_toolbar as toolbar

  val mutable position = m_position

  val mutable size = (m_width, 0)

  val mutable nb_items = 0

  val mutable item_height = m_item_height

  val mutable theme = m_theme

  val mutable toolbar_height = m_bar_height

  val mutable toolbar_icon = m_bar_icon

  val mutable toolbar_text = m_bar_text

  val mutable escape = escape

  method set_escape (esc : unit -> unit) = escape <- Some esc

  method draw target lib = if self#active then begin
    let shape =
      let position, size = OgamlMath.Vector2f.(
        (let (x,y) =
          foi2D (fst self#position, snd self#position-m_bar_height+2)
        in {x;y}),
        (let (x,y) = foi2D (fst size, snd size+m_bar_height-2) in {x;y})
      ) in
      Shape.create_rectangle
        ~position ~size ~color:theme.Theme.default_color
        ~thickness:2. ~border_color:theme.Theme.border_color ()
    in
    Shape.draw (module Window) target shape () ;
    toolbar#draw target lib ;
    let (posx, posy) = self#position in
    let shape =
      let position, size = OgamlMath.Vector2f.(
        (let (x,y) = foi2D (posx, posy + self#selected * item_height) in {x;y}),
        (let (x,y) = foi2D (m_width, item_height) in {x;y})
      ) in
      Shape.create_rectangle
        ~position ~size ~color:theme.Theme.highlight_color ()
    in
    Shape.draw (module Window) target shape () ;
    List.iter (fun w -> w#draw target lib) self#children
  end

  method add_child w =
    super#add_child w;
    nb_items <- nb_items + 1

  method clear_children =
    nb_items <- 0 ;
    super#clear_children

  method toggle =
    super#toggle ;
    toolbar#toggle ;
    kcl#reset_selection


  initializer
    self#add_event(OgamlCore.(function
      | Event.KeyPressed { Event.KeyEvent.key = Keycode.Escape ; _ } ->
          begin match escape with
            | Some esc -> esc () ; true
            | None -> false
          end
      | Event.KeyPressed { Event.KeyEvent.key = Keycode.Return ; _ }
      | Event.KeyPressed { Event.KeyEvent.key = Keycode.Space ; _ } ->
          nb_items <> 0 &&
          ((List.nth self#children (nb_items - self#selected - 1))#action;
          true)
      | _ -> false))

end
