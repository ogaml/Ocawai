open OgamlGraphics
open GuiTools
open Widget
open BaseMixins
open Utils

let my_font = Font.load "FreeSans.ttf"

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
    (* Render.renderer#draw_txr target icon ~position:position
      ~size:(selfy, selfy) ~centered:false ~color ();
    (* Then draw the text *)
    rect_print
      target text my_font
        (if enabled then Color.black else Color.rgba 0 0 0 127)
        (Pix (snd size - 3)) (Pix 2) Left {
        left = fst position +. selfy ;
        top = snd position ;
        width = selfx -. selfy ;
        height = selfy } *)
    () (* TODO *)
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
    (* new rectangle_shape ~fill_color:theme.Theme.default_color
      ~size:(foi2D size) ~position:(foi2D self#position)
      ~outline_color:theme.Theme.border_color
      ~outline_thickness:2. ()
    |> target#draw;

    let (selfx, selfy) = foi2D size in
    let position = foi2D self#position in
    Render.renderer#draw_txr target icon ~position ~size:(selfy, selfy)
      ~centered:false ();

    rect_print
      target text my_font Color.black (Pix (snd size - 1)) (Pix 2) Center {
        left = fst position +. selfy ;
        top = snd position ;
        width = selfx -. selfy ;
        height = selfy } *)
        () (* TODO *)
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
    (* new rectangle_shape ~fill_color:theme.Theme.default_color
      ~size:(foi2D (fst size, snd size+m_bar_height-2))
      ~position:(foi2D (fst self#position, snd self#position-m_bar_height+2))
      ~outline_thickness:2. ~outline_color:theme.Theme.border_color ()
    |> target#draw;
    toolbar#draw target lib;
    let (posx, posy) = self#position in
    new rectangle_shape ~fill_color:(theme.Theme.highlight_color)
      ~size:(foi2D (m_width, item_height))
      ~position:(foi2D (posx, posy + self#selected * item_height)) ()
    |> target#draw;
    List.iter (fun w -> w#draw target lib) self#children *)
    () (* TODO *)
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
