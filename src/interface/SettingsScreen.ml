open OgamlGraphics
open OgamlCore
open OgamlMath
open OgamlUtils
open Utils
open GuiTools
open Settings_interface_t

open Manager

class state = object(self)

  inherit State.state as super

  val mutable screen = new Home.screen [] []

  val font = Font.load "resources/fonts/FreeSansBold.ttf"

  method private set_screen size =
    let sizef = Vector2f.from_int size in
    let (w,h) = Vector2f.(sizef.x, sizef.y) in
    screen <- new Home.screen
      []
      [
        (new Setters.slider (w /. 2., 150.)
          ~default:(int_of_float ((Config.config#settings_interface.cursor_speed -. 1.) *. (19. /. 50.)))
          (fun i ->
            Config.config#settings_interface.cursor_speed <- (1. +. (50. /. 19.) *. (float_of_int i)))
          "Cursor speed" :> Home.actionnable) ;
        (new Setters.slider (w /. 2., 150. +. Setters.setter_height)
          ~default:(int_of_float ((Config.config#settings_interface.zoom_speed -. 1.) *. (9. /. 50.)))
          (fun i ->
            Config.config#settings_interface.zoom_speed <- (1. +. (50. /. 9.) *. (float_of_int i)))
          "Zoom speed" :> Home.actionnable) ;
        (* TODO *)
        (*(new Setters.slider (w /. 2., 150. +. 2. *. Setters.setter_height)
          ~default: (int_of_float (Sounds.get_volume ()))
          (fun i ->
            Sounds.play_sound "click";
            Sounds.set_volume (float_of_int i))
            "Sounds volume" :> Home.actionnable) ;*)
        (* TODO *)
        (*(new Setters.slider (w /. 2., 150. +. 3. *. Setters.setter_height)
          ~default: (MidiPlayer.get_volume ())
          (fun i ->
            MidiPlayer.set_volume i)
            "Music volume" :> Home.actionnable) ;*)
        (new Setters.toogle (w /. 2., 150. +. 4. *. Setters.setter_height)
          "Fullscreen"
          ~default: true
          manager#set_fullscreen :> Home.actionnable) ;
        new Home.textured_actionnable "back" "back_hover" (200., h -. 100.)
          (fun () -> manager#pop) ;
      ]

  method handle_event e =

    Event.(
      match e with
        | KeyPressed { KeyEvent.key = kc ; _ } ->
            screen#handle_key kc
        | _ -> ()
    )

  method render window =

    let color = `RGB Color.RGB.({r = 221. /. 255.; g = 224. /. 255.; b = 234. /. 255.; a = 1.0}) in
    Window.clear ~color:(Some color) window;

    let wsize = Vector2f.from_int (Window.size window) in

    rect_print
      (module Window) window "SETTINGS" font (`RGB Color.RGB.black) (Pix 70) (Pix 10) Left
      FloatRect.({ x = 10. ; y = 10. ; width = wsize.Vector2f.x -. 20. ; height = 100. });

    screen#draw window;

    Window.display window

  initializer
    let window = manager#window in
    let wsize = Window.size window in
    self#set_screen wsize

  method destroy =
    ()

end
