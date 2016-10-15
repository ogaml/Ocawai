open OgamlGraphics
open OgamlCore
open OgamlMath
open Utils
open GuiTools

open Manager

class main_menu = object(self)

  inherit State.state as super

  val mutable screen = new Home.screen [] []

  val bg_texture = 
    Texture.Texture2D.create 
      (module Window) manager#window 
      (`File ((Utils.base_path ()) ^ "textures/gui/capture.png"))
  val mutable bg_offset = (0.,0.)
  val mutable bg_dir = (0.,0.)
  val mutable music_run = ref true

  val key_seq = Keycode.([
    Up;
    Up;
    Down;
    Down;
    Left;
    Right;
    Left;
    Right;
    B;
    A
  ])

  val mutable remaining_keys = Keycode.([
    Up;
    Up;
    Down;
    Down;
    Left;
    Right;
    Left;
    Right;
    B;
    A
  ])

  method private update_offset =
    bg_offset <- Utils.addf2D bg_offset bg_dir;
    let ox, oy = bg_offset in
    if ox <= 0. && oy <= 0. then
      bg_dir <- (0.22, 0.)
    else if ox >= 200. && oy <= 0. then
      bg_dir <- (0., 0.22)
    else if ox >= 200. && oy >= 200. then
      bg_dir <- (-0.22, 0.)
    else if ox <= 0. && oy >= 200. then
      bg_dir <- (0., -0.22)


  method private handle_keys e =
    match remaining_keys with
    | key :: r -> begin
        Event.(match e with
        | KeyPressed { KeyEvent.key = k ; _ } when k = key -> remaining_keys <- r
        | KeyPressed _ -> remaining_keys <- key_seq
        | _ -> ())
      end
    | [] ->
        (new Snake.state :> State.state) |> manager#push ;
        remaining_keys <-key_seq

  method private set_screen wsize =
    let wsizef = Vector2f.from_int wsize in
    let (w,h) = 
      Vector2f.(wsizef.x, wsizef.y)
    in
    screen <- new Home.screen
      [new Home.textured_item "title" (w/.2., h /. 2. -. 250.)]
      [
        new Home.textured_actionnable "gameon" "gameon_hover"
          (w/.2., h /. 2. +. 30.)
          (fun () -> new CharacterScreen.state |> manager#push) ;
        new Home.textured_actionnable "quit" "quit_hover"
          (w /. 2. -. 130., h /. 2. +. 230.)
          (fun () -> Window.close manager#window) ;
        new Home.textured_actionnable "settings" "settings_hover"
          (w /. 2. +. 100., h /.2. +. 220.)
          (fun () -> new SettingsScreen.state |> manager#push) ;
        new Home.textured_actionnable "credits" "credits_hover"
          (w /. 2. -. 50., h -. 50.)
          (fun () -> new Credits.state |> manager#push)
      ]

  method handle_event e =

    self#handle_keys e;

    Event.(
      match e with
        | Resized size -> self#set_screen (Window.size manager#window)
        | KeyPressed { KeyEvent.key = kc ; _ } ->
            screen#handle_key kc 
        | _ -> ()
    )

  method render window =

    Interpolators.update ();

    self#update_offset;

    let color = `RGB Color.RGB.({r = 221. /. 255.; g = 224. /. 255.; b = 234. /. 255.; a = 1.0}) in
    Window.clear ~color:(Some color) window;

    let (w,h) = 
      (Window.size window) 
      |> Vector2f.from_int 
      |> (fun v -> Vector2f.(v.x, v.y))
    in
    let (tw,th) = 
      Texture.Texture2D.size bg_texture
      |> Vector2f.from_int 
      |> (fun v -> Vector2f.(v.x, v.y))
    in
    let sprite = 
      Sprite.create
        ~texture:bg_texture 
        ~scale:Vector2f.({x = w *. 1.5 /. tw; y = h *. 1.5 /. th})
        ~position:Vector2f.({x = -. (fst bg_offset); y = -. (snd bg_offset)}) ()
    in
    Sprite.draw (module Window) ~target:window ~sprite ();

    screen#draw window;

    Window.display window

  method paused =
    music_run := false

  method resumed =
    music_run := true

  initializer
    let window = manager#window in
    let wsize = Window.size window in 
    self#set_screen wsize; 
    (* TODO *)
    (*let music_player = MusicPlayer.music_player () in
    ignore @@ Thread.create (music_player#play_menu) (music_run)*)

end
