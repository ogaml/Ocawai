open OgamlGraphics
open OgamlMath
open OgamlUtils
open OgamlCore
open Utils
open GuiTools

open Manager

class state = object(self)

  inherit State.state as super

  val mutable screen = new Home.screen [] []

  val font = Font.load "resources/fonts/FreeSansBold.ttf"

  method private set_screen size =
    let (w,h) = 
      Vector2f.from_int size
      |> (fun v -> Vector2f.(v.x, v.y))
    in
    screen <- new Home.screen
      []
      [
        (* Choosing flatman *)
        new Home.textured_actionnable "flatman" "flatman_hover"
          (w/.2., h /. 2. +. 30.)
          (fun () -> manager#pop ; manager#push
            (new LoadScreen.state
              (Game.new_game ~character:Characters.Flatman) :> State.state)) ;
        (* Choosing Blub as main character *)
        new Home.textured_actionnable "blub" "blub_hover"
          (w/.2. -. 220., h /. 2. +. 30.)
          (fun () -> manager#pop ; manager#push
            (new LoadScreen.state
              (Game.new_game ~character:Characters.Blub) :> State.state)) ;
        (* Or limboy *)
        new Home.textured_actionnable "limboy" "limboy_hover"
          (w/.2. +. 220., h /. 2. +. 30.)
          (fun () -> manager#pop ; manager#push
            (new LoadScreen.state
              (Game.new_game ~character:Characters.Limboy) :> State.state)) ;
        (* Or you don't care *)
        new Home.textured_actionnable "random" "random_hover"
          (w -. 200., h -. 100.)
          (fun () -> manager#pop ; manager#push
            (new LoadScreen.state
              (Game.new_game) :> State.state)) ;
        (* Back button *)
        new Home.textured_actionnable "back" "back_hover" (200., h -. 100.)
          (fun () -> manager#pop)
      ]

  method handle_event e =

    Event.(
      match e with
        | Resized size -> self#set_screen size
        | KeyPressed { KeyEvent.key = kc ; _ } ->
            screen#handle_key kc
        | _ -> ()
    )

  method render window =

    let color = `RGB Color.RGB.({r = 221. /. 255.; g = 224. /. 255.; b = 234. /. 255.; a = 1.0}) in
    Window.clear ~color:(Some color) window;

    let (w,h) = 
      Window.size window
      |> Vector2f.from_int
      |> (fun v -> Vector2f.(v.x, v.y))
    in

    rect_print
      (module Window) window "CHOOSE YOUR CHARACTER" font (`RGB Color.RGB.black) (Pix 70) (Pix 10) Center
      FloatRect.({ x = 10. ; y = 30. ; width = w -. 20. ; height = 100. });

    rect_print
      (module Window) window "And choose wisely because it only changes the textures."
      font (`RGB Color.RGB.({r = 0.; g = 0.; b = 0.; a = 0.4})) (Pix 27) (Pix 10) Center
      FloatRect.({ x = 10. ; y = 95. ; width = w -. 20. ; height = 50. });

    screen#draw window;

    Window.display window

  initializer
    let window = manager#window in
    self#set_screen (Window.size window)

end
