open OgamlGraphics
open Utils
open GuiTools
open Widget

(* Put it in the theme ? *)
let bold_font = Font.load "resources/fonts/FreeSansBold.ttf"


class virtual ['a] widget_container = object(self)

  inherit widget as super

  constraint 'a = #widget

  val mutable children : 'a list = []

  method add_child (w : 'a) =
    children <- w::children;
    w#set_parent (Some (self :> widget));
    self#add_event (fun e -> w#on_event e)

  method children = children

  method toggle =
    super#toggle;
    List.iter (fun c -> c#toggle) children

  method clear_children =
    children <- []


end


class virtual themed_widget = object(self)

  val virtual mutable theme : Theme.t

end


class virtual ['a] evq_container = object(self)

  inherit ['a] widget_container as super

  val virtual mutable item_height : int

  method add_child w =
    super#add_child w;
    size <- (fst size, snd size + item_height);
    w#set_size (fst size, item_height);
    w#set_position (0, (List.length children - 1) * item_height)

  method clear_children =
    List.iter (fun w -> size <- (fst size, snd size - item_height)) children;
    super#clear_children


end



class virtual key_ctrl_list key1 key2 = object(self)

  val mutable selected = 0

  val virtual mutable nb_items : int

  method virtual add_event : (OgamlCore.Event.t -> bool) -> unit

  method selected = selected

  method reset_selection = selected <- 0

  initializer
    self#add_event OgamlCore.(function
      | Event.KeyPressed { Event.KeyEvent.key = kc ; _} when kc = key1 ->
          nb_items <> 0
          && (selected <- (selected - 1 + nb_items) mod nb_items; true)
      | Event.KeyPressed { Event.KeyEvent.key = kc ; _} when kc = key2 ->
          nb_items <> 0
          && (selected <- (selected + 1 + nb_items) mod nb_items; true)
      | _ -> false)
end


class virtual has_toolbar = object(self)

  inherit Widget.widget

  inherit themed_widget

  val virtual mutable toolbar_height : int

  val virtual mutable toolbar_icon : string

  val virtual mutable toolbar_text : string

  method draw target lib =
    let position = foi2D (sub2D self#position (2, toolbar_height)) in
    let position =
      let (x,y) = position in OgamlMath.Vector2f.({ x ; y })
    in
    let shape =
      let size = foi2D (fst size + 4, toolbar_height) in
      let size = let (x,y) = size in OgamlMath.Vector2f.({ x ; y }) in
      Shape.create_rectangle
        ~position ~size
        ~color:theme.Theme.bar_color ()
    in
    Shape.draw (module Window) target shape () ;
    let th = float_of_int toolbar_height in
    let size = OgamlMath.Vector2f.({ x = th ; y = th }) in
    Render.renderer#draw_txr
      target toolbar_icon ~position ~size ~centered:false () ;
    let (sx,sy) =
      foi2D (int_of_float OgamlMath.Vector2f.(size.x), toolbar_height)
    in
    let (px,py) = OgamlMath.Vector2f.(position.x, position.y) in
    rect_print (module Window)
      target toolbar_text bold_font Color.(`RGB RGB.white)
      (Pix (toolbar_height - 3)) (Pix 2) Left OgamlMath.FloatRect.({
        x = px +. th ;
        y = py ;
        width = sx -. th ;
        height = sy
      })

end
