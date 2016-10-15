open OgamlGraphics
open OgamlMath
open OgamlUtils
open Utils
open GuiTools

open Manager

class state (build : unit -> State.state) = object(self)

  inherit State.state as super

  val font = (Font.load "resources/fonts/FreeSansBold.ttf")

  val mutable init = false

  val mutable text_alpha = 1.

  method private set_alpha a =
    text_alpha <- a

  method render window =

    (* On first render we load in parallel the other screen *)
    if not init then begin
      let _ =
        Thread.create
          (fun () ->
            try let s = build () in manager#pop ; manager#push s
            with
            | Config.Missing_config(msg) as e-> manager#pop ;manager#pop ; raise e
            | e -> manager#pop ; raise e) ()
        in init <- true
    end;

    Interpolators.update ();

    let color = Color.RGB.({r = 19./.255.; g = 42./.255.; b = 69./.255.; a = 1.0}) in
    Window.clear window ~color:(Some (`RGB color));

    let (w,h) = 
      Window.size window 
      |> Vector2f.from_int
      |> (fun v -> Vector2f.(v.x, v.y))
    in

    let text_color =
      Color.RGB.({r = 1.0; g = 1.0; b = 1.0; a = text_alpha})
    in
    rect_print
      (module Window) window "LOADING" font (`RGB text_color) (Pix 120) (Pix 10) Center
      FloatRect.({ x= 0. ; y = h /. 2. -. 100. ; width = w ; height = 100. });

    Window.display window

  initializer
    ignore(Interpolators.new_sine_ip
      self#set_alpha 2. 0.4 0.6);

  method destroy =
    ()

end
