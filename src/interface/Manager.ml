open OgamlGraphics
open OgamlCore

let manager = 

  let window : OgamlGraphics.Window.t =
    let settings = 
      ContextSettings.create
        ~depth:0
        ~fullscreen:true ()
    in
    Window.create ~width:800 ~height:600 ~title:"OCAWAI" ~settings ()
  in

  object(self)

  val mutable states : State.state list = []
  val mutable fullscreen = true
  val mutable window = window

  initializer
    Render.renderer#init window
(*    TODO Sounds.load_sounds () *)

  method window : OgamlGraphics.Window.t = window

  method reset_window =
    Window.destroy window;
    let settings = 
      ContextSettings.create
        ~depth:0
        ~fullscreen ()
    in
    window <- Window.create ~width:800 ~height:600 ~title:"OCAWAI" ~settings ()

  method set_fullscreen b =
    fullscreen <- b ;
    self#reset_window

  method push (state : State.state) =
    if self#is_running then self#current#paused ;
    states <- state :: states

  method pop =
    self#current#destroy ;
    states <- List.tl states ;
    if self#is_running then self#current#resumed

  method current = List.hd states

  method is_running = states <> []

  method event_loop =
    if self#is_running then
    match Window.poll_event window with
    | Some e ->
        OgamlCore.Event.(
        begin match e with
          | Closed
          | KeyPressed { KeyEvent.key = Keycode.Q ; control = true ; _ }
          | KeyPressed { KeyEvent.key = Keycode.C ; control = true ; _ } ->
              Window.close window

          | _ -> self#current#handle_event e
        end) ;
        self#event_loop
    | None -> ()

  method run =

    if not (Window.is_open window) then while self#is_running do self#pop done ;

    if self#is_running then self#event_loop ;
    if self#is_running then
    begin
      self#current#render window ;
      self#run
    end

end
