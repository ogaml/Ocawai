open OgamlGraphics
open GuiTools
open Widget
open BaseMixins
open Utils
open Menus

let my_font = Font.load "resources/fonts/FreeSans.ttf"

class text_framed_item m_position m_size m_text
  (action : unit -> unit) m_theme = object(self)

  inherit widget

  inherit themed_widget

  val mutable position = m_position

  val mutable size = m_size

  val mutable theme = m_theme

  method draw target lib = if self#active then begin
    let position = foi2D self#position in
    let position, size = OgamlMath.Vector2f.(
      (let (x,y) = position in { x ; y }),
      (let (x,y) = foi2D size in { x ; y })
    ) in
    let shape =
      Shape.create_rectangle
        ~position ~size ~color:(Color.(`RGB RGB.transparent))
        ~thickness:2. ~border_color:theme.Theme.border_color ()
    in
    Shape.draw (module Window) target shape () ;
    let x,y,width,height = OgamlMath.Vector2f.(
      position.x, position.y, size.x, size.y
    ) in
    rect_print (module Window)
      target m_text my_font (Color.(`RGB RGB.black))
      (Pix ((int_of_float height) - 3))
      (Pix 2) Center OgamlMath.FloatRect.({ x ; y ; width ; height })
  end

  method action = action ()

end


class ingame_popup ~m_position ~m_size ~m_theme ~m_text ~m_bar_height
  ~m_bar_icon ~m_bar_text = object(self)

    inherit [text_framed_item] widget_container as super

    inherit key_ctrl_list OgamlCore.Keycode.Left OgamlCore.Keycode.Right

    inherit has_toolbar as toolbar

    val mutable nb_items = 0

    val mutable position = m_position

    val mutable size = m_size

    val mutable theme = m_theme

    val mutable toolbar_height = m_bar_height

    val mutable toolbar_text = m_bar_text

    val mutable toolbar_icon = m_bar_icon

    method add_child w =
      super#add_child w;
      nb_items <- nb_items + 1

    method draw target lib = if self#active then begin
      let active_widget = List.nth self#children self#selected in
      let shape =
        let position, size = OgamlMath.Vector2f.(
          (let (x,y) =
            foi2D (fst self#position, snd self#position-m_bar_height+2)
          in { x ; y }),
          (let (x,y) = foi2D (fst size, snd size+m_bar_height-2) in { x ; y })
        ) in
        Shape.create_rectangle
          ~position ~size ~color:theme.Theme.default_color
          ~thickness:2. ~border_color:theme.Theme.border_color ()
      in
      Shape.draw (module Window) target shape () ;
      toolbar#draw target lib ;
      let position, size = OgamlMath.Vector2f.(
        (let (x,y) = foi2D active_widget#position in { x ; y }),
        (let (x,y) = foi2D active_widget#get_size in { x ; y })
      ) in
      let shape =
        Shape.create_rectangle
          ~position ~size ~color:theme.Theme.highlight_color ()
      in
      Shape.draw (module Window) target shape () ;
      let x,y,width,height = OgamlMath.Vector2f.(
        position.x, position.y, size.x, size.y
      ) in
      rect_print (module Window)
        target m_text my_font (Color.(`RGB RGB.black)) (Pix 18) (Pix 2) Center
        OgamlMath.FloatRect.({ x ; y ; width ; height }) ;
      List.iter (fun w -> w#draw target lib) self#children
    end

  initializer
    self#add_event(OgamlCore.(function
      | Event.KeyPressed { Event.KeyEvent.key = Keycode.Return ; _ }
      | Event.KeyPressed { Event.KeyEvent.key = Keycode.Space ; _ } ->
          nb_items <> 0 &&
          ((List.nth self#children self#selected)#action;
          true)
      | _ -> false))

end
