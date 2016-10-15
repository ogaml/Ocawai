open OgamlGraphics
open OgamlCore
open OgamlUtils
open OgamlMath
open Utils
open GuiTools

open Manager

class state = object(self)

  inherit State.state as super

  val font = Font.load "resources/fonts/FreeSansBold.ttf"

  val map = Array.make_matrix 16 10 false
  val goods = Array.make_matrix 16 10 false

  val mutable running = true

  val mutable current_pos = Position.create (0,0)
  val mutable snake = [Position.create (0,0)]
  val mutable size = 5

  val tl = Position.create (0,0)
  val br = Position.create (15,9)

  val mutable musicThread = None
  val mutable runMusic = ref true

  val mutable last_move = (1,0)
  val mutable move_dir = (1,0)

  method private move =
    let new_pos = add2D (Position.topair current_pos) move_dir in
    current_pos <- Position.create new_pos;
    last_move <- move_dir;
    if Position.out_of_bounds current_pos tl br then
      running <- false
    else begin
      let (x,y) = Position.topair current_pos in
      if map.(x).(y) then
        running <- false
      else begin
        map.(x).(y) <- true;
        if goods.(x).(y) then begin
          size <- size + 1;
          goods.(x).(y) <- false
        end;
        snake <- current_pos :: snake;
        if List.length snake = (size + 1) then
        begin
          let rs = List.rev snake in
          let (x,y) = Position.topair (List.hd rs) in
          map.(x).(y) <- false;
          snake <- List.rev (List.tl rs)
        end
      end
    end

  (* Inspired by @VLanvin *)
  val mutable last_event = 0.

  method private handle_keys =
    let act_time = Unix.gettimeofday () in
    if act_time -. last_event >= 0.1 then (
      last_event <- act_time;
      self#move;
      let p = Random.int 100 in
      if p >= 95 then
      begin
        let x = Random.int 16
        and y = Random.int 10 in
        if not map.(x).(y) then goods.(x).(y) <- true
      end
    )

  method private topos pos =
    let (w,h) = 
      Window.size manager#window
      |> Vector2f.from_int
      |> (fun v -> Vector2f.(v.x, v.y))
    in
    let dx = w /. 2. -. 400. +. 25.
    and dy = h /. 2. -. 250. +. 25. in
    let (x,y) = foi2D (Position.topair pos) in
    (x *. 50. +. dx, y *. 50. +. dy)

  method private draw_path (target : Window.t) path =
    let draw pos rot name = 
      let position = 
        let (x,y) = self#topos pos in
        Vector2f.({x;y})
      in
      Render.renderer#draw_txr target name
        ~position ~rotation:rot () 
    in
    let angle s t =
      match Position.diff t s with
        | pos when pos = Position.create (1,0)  -> 0.
        | pos when pos = Position.create (0,1)  -> Constants.pi2
        | pos when pos = Position.create (-1,0) -> Constants.pi
        | pos when pos = Position.create (0,-1) -> -. Constants.pi2
        | _ -> failwith "Not continuous path"
    in
    let rec aux prev = function
      | pos :: [] -> draw pos (angle prev pos) "arrow_end"
      | pos :: next :: r ->
        let ap = angle pos prev
        and an = angle pos next in
        let (amax,amin) = if an > ap then (an,ap) else (ap,an) in
        if amax = amin +. Constants.pi then
          draw pos ap "arrow_straight"
        else begin
          if amax = amin +. (3. *. Constants.pi2) then
            draw pos (3. *. Constants.pi2) "arrow_corner"
          else
            draw pos amax "arrow_corner"
        end ;
        aux pos (next :: r)
      | [] -> ()
    in
    match path with
      | start :: [] -> draw start 0. "arrow_lone"
      | start :: next :: r ->
        draw start (angle start next) "arrow_start" ;
        aux start (next :: r)
      | [] -> ()

  method handle_event e =

    Event.(
      match e with
        | KeyPressed { KeyEvent.key = Keycode.Space ; _ } ->
            if not running then begin
              for i = 0 to 15 do
                for j = 0 to 9 do
                  map.(i).(j) <- false;
                  goods.(i).(j) <- false;
                done
              done;
              snake <- [Position.create (0,0)];
              current_pos <- Position.create (0,0);
              move_dir <- (1,0);
              last_move <- (1,0);
              size <- 5;
              running <- true
            end
        | KeyPressed { KeyEvent.key = Keycode.Right ; _ } ->
            if last_move = (-1,0) then ()
            else move_dir <- (1,0)
        | KeyPressed { KeyEvent.key = Keycode.Left ; _ } ->
            if last_move = (1,0) then ()
            else move_dir <- (-1,0)
        | KeyPressed { KeyEvent.key = Keycode.Up ; _ } ->
            if last_move = (0,1) then ()
            else move_dir <- (0,-1)
        | KeyPressed { KeyEvent.key = Keycode.Down ; _ } ->
            if last_move = (0,-1) then ()
            else move_dir <- (0,1)
        | KeyPressed { KeyEvent.key = Keycode.Escape ; _ }
        | KeyPressed { KeyEvent.key = Keycode.Q ; _ } ->
            manager#pop
        | _ -> ()
    )

  method render window =

    let color = `RGB Color.RGB.({r = 19. /. 255.; g = 42. /. 255.; b = 69. /. 255.; a = 1.0}) in
    Window.clear ~color:(Some color) window;

    let (w,h) = 
      Window.size manager#window
      |> Vector2f.from_int
      |> (fun v -> Vector2f.(v.x, v.y))
    in

    (* Bounding area *)
    let rectangle = 
      Shape.create_rectangle
        ~position:(Vector2f.prop 0.5 (Vector2f.from_int (Window.size window)))
        ~size:Vector2f.({x = 800.; y = 500.})
        ~border_color:(`RGB Color.RGB.red)
        ~thickness:5.
        ~origin:Vector2f.({x = 400.; y = 250.})
        ~color
        ()
    in
    Shape.draw (module Window) ~target:window ~shape:rectangle ();
    self#draw_path window (List.rev snake);

    (* Drawing candies *)
    for x = 0 to 15 do
      for y = 0 to 9 do
        if goods.(x).(y) then
          let (x,y) = self#topos (Position.create (x,y)) in
          let position = Vector2f.({x;y}) in
          Render.renderer#draw_txr window "flatman_infantry" ~position ~rotation:(Random.float 360.) ()
      done
    done;

    if running then
      self#handle_keys
    else begin
      rect_print (module Window)
        window "GAME OVER" font (`RGB Color.RGB.white) (Pix 120) (Pix 10) Center
        FloatRect.({ x = 0. ; y = h /. 2. -. 100. ; width = w ; height = 100. });
      rect_print (module Window)
        window "Press space to continue, q to quit."
        font (`RGB Color.RGB.white) (Pix 50) (Pix 10) Center
        FloatRect.({ x = 0. ; y = h /. 2. +. 185. ; width = w ; height = 100. });
    end;

    Window.display window 

  initializer
    Random.self_init ();
    (* TODO *)
    (*Sounds.play_sound "lets_do_this";
    let tetris_path = (Utils.base_path ()) ^ "music/tetris.mid" in
    musicThread <-
      Some (Thread.create (fun x -> Thread.delay 1. ; MidiPlayer.play_midi_file tetris_path x) runMusic)*)


  method destroy =
    runMusic := false

end
