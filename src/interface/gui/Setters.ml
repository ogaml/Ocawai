open OgamlGraphics

open Home
open GuiTools
open Utils

let font = Font.load "FreeSans.ttf"

let setter_width = 800.
let setting_width = 300.
let setter_height = 40.

class virtual setter pos name = object(self)

  inherit modal

  method position = pos

  method draw (target : OgamlGraphics.Window.t) =

    let bg_color =
      if self#holds_focus then Color.(`RGB { .9 ; .9 ; .9 ; 1. })
      else if has_focus then Color.(`RGB { .9 ; .94 ; .95 ; 1. })
      else Color.(`RGB RGB.white)
    in
    (* new rectangle_shape ~fill_color:bg_color
      ~size:(setter_width,setter_height) ~position:self#position
      ~origin:(setter_width/.2.,setter_height/.2.)
      ()
    |> target#draw ; *)

    rect_print
      target name font (Color.rgb 64 64 64) (Pix 20) (Pix 2) Left {
        left = fst self#position +. 20. -. 400. ;
        top = snd self#position +. 10. -. 20. ;
        width = setter_width -. setting_width -. 4. ;
        height = setter_height }

end

class slider ?default:(default = 50) pos update name = object(self)

  inherit setter pos name as super_set

  val slider_h = 2.
  val slider_w = setting_width -. 30.

  val cursor_r = 7.

  val mutable percentage = default

  method draw (target : OgamlGraphics.Window.t) =

    super_set#draw target ;
    (* First we have a line for the slider *)
    (* let color = Color.rgb 57 131 204 in
    new rectangle_shape ~fill_color:color
      ~size:(slider_w,slider_h)
      ~origin:(slider_w /. 2., slider_h /. 2.)
      ~position:(addf2D self#position ((setter_width -. setting_width) /. 2., 0.))
      ()
    |> target#draw ;
    (* Then we draw the cursor *)
    let offset = (slider_w /. 100.) *. (float_of_int (percentage - 50)) in
    let position =
      addf2D self#position
             ((setter_width -. setting_width) /. 2. +. offset, 0.) in
    new circle_shape
      ~fill_color: color
      ~radius: cursor_r
      ~origin: (cursor_r, cursor_r)
      ~position
      ~point_count: 100
      ()
    |> target#draw *)
    ()
    (* TODO *)

  method private incr =
    percentage <- min (percentage + 1) 100

  method private decr =
    percentage <- max 0 (percentage - 1)

  method action =
    holds_focus <- true

  method handle_key = OgamlCore.Keycode.(function
    | Left -> self#decr
    | Right -> self#incr
    | Escape | Space | Return ->
        update percentage ;
        holds_focus <- false
    | _ -> ()
  )

end

class toogle ?default:(default = false) pos name update = object(self)

  inherit setter pos name as super

  val led_r = 8.

  val mutable toogle = default

  method draw (target : OgamlGraphics.Window.t) =
    super#draw target ;
    (* let fill_color = if toogle then Color.rgb 57 131 204 else Color.white
    and position = addf2D (setter_width/.2. -. 20., 0.) self#position
    and outline_color = Color.rgb 97 171 244
    and outline_thickness = 2. in
    new circle_shape
    ~fill_color
    ~radius: led_r
    ~origin: (led_r, led_r)
    ~position
    ~point_count: 100
    ~outline_color
    ~outline_thickness
    ()
    |> target#draw *)
    ()
    (* TODO *)

  method action =
    toogle <- not toogle ;
    update toogle

end
