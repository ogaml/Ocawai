open OgamlGraphics

open Home
open GuiTools
open Utils

let font = Font.load "resources/fonts/FreeSans.ttf"

let setter_width = 800.
let setting_width = 300.
let setter_height = 40.

class virtual setter pos name = object(self)

  inherit modal

  method position = pos

  method draw (target : OgamlGraphics.Window.t) =

    let bg_color =
      if self#holds_focus then Color.(`RGB RGB.{ r = 0.9 ; g = 0.9 ; b = 0.9 ; a = 1. })
      else if has_focus then Color.(`RGB RGB.{ r = 0.9 ; g = 0.94 ; b = 0.95 ; a = 1. })
      else Color.(`RGB RGB.white)
    in
    let shape =
      let position =
        let (x,y) = self#position in OgamlMath.Vector2f.({ x ; y })
      in
      let size =
        OgamlMath.Vector2f.({ x = setting_width ; y = setter_height })
      in
      let origin =
        OgamlMath.Vector2f.({ x = setting_width/.2. ; y = setter_height/.2.})
      in
      Shape.create_rectangle ~position ~size ~color:bg_color ~origin ()
    in
    Shape.draw (module Window) target shape () ;

    rect_print (module Window)
      target name font
      (Color.(`RGB RGB.({ r = 0.25 ; g = 0.25 ; b = 0.25 ; a = 1. })))
      (Pix 20) (Pix 2) Left OgamlMath.FloatRect.({
        x = fst self#position +. 20. -. 400. ;
        y = snd self#position +. 10. -. 20. ;
        width = setter_width -. setting_width -. 4. ;
        height = setter_height
      })

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
    let color = Color.(`RGB RGB.({ r = 0.22 ; g = 0.51 ; b = 0.8 ; a = 1. })) in
    let shape =
      let position, size, origin =
      OgamlMath.Vector2f.(
        let (x,y) =
          addf2D self#position ((setter_width -. setting_width) /. 2., 0.)
        in
        { x ; y },
        { x = slider_w ; y = slider_h },
        { x = slider_w /. 2. ; y = slider_h /. 2. }
      ) in
      Shape.create_rectangle ~position ~origin ~size ~color ()
    in
    Shape.draw (module Window) target shape () ;
    (* Then we draw the cursor *)
    let offset = (slider_w /. 100.) *. (float_of_int (percentage - 50)) in
    let position =
      addf2D self#position
             ((setter_width -. setting_width) /. 2. +. offset, 0.)
    in
    let shape =
      let position, origin = OgamlMath.Vector2f.(
        let (x,y) = position in { x ; y },
        { x = cursor_r ; y = cursor_r }
      ) in
      Shape.create_regular
        ~position ~radius:cursor_r ~amount:100 ~color ~origin ()
    in
    Shape.draw (module Window) target shape ()

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
    let color = Color.(`RGB RGB.(
      if toogle then { r = 0.22 ; g = 0.51 ; b = 0.8 ; a = 1.} else white
    ))
    and position = addf2D (setter_width/.2. -. 20., 0.) self#position
    and border_color =
      Color.(`RGB RGB.({r = 0.38 ; g = 0.67 ; b = 0.95 ; a = 1. }))
    and thickness = 2. in
    let shape =
      let position, origin = OgamlMath.Vector2f.(
        let (x,y) = position in { x ; y },
        { x = led_r ; y = led_r }
      ) in
      Shape.create_regular
        ~position ~radius:led_r ~amount:100 ~color ~origin
        ~border_color ~thickness ()
    in
    Shape.draw (module Window) target shape ()

  method action =
    toogle <- not toogle ;
    update toogle

end
