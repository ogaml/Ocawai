open Utils

class virtual item = object(self)

  method virtual draw : OgamlGraphics.Window.t -> unit
  method virtual position : float * float

  method x = fst self#position
  method y = snd self#position

  method holds_focus = false
  method handle_key (key : OgamlCore.Keycode.t) = ()

end

class virtual actionnable = object(self)

  inherit item as super

  val mutable has_focus = false

  method set_focus b = has_focus <- b

  method virtual action : unit

end

class virtual modal = object(self)

  inherit actionnable

  val mutable holds_focus = false

  method holds_focus = holds_focus
  method virtual handle_key : OgamlCore.Keycode.t -> unit

end

class textured_item name position = object(self)

  inherit item

  method draw target =
    let position =
      let (x,y) = position in OgamlMath.Vector2f.({ x ; y })
    in
    Render.renderer#draw_txr target name ~position ()

  method position = position

end

class textured_actionnable txr txr_hover position (action : unit -> unit) = object(self)

  inherit textured_item txr position as super
  inherit actionnable

  method action = action ()

  method draw target =
    let position =
      let (x,y) = position in OgamlMath.Vector2f.({ x ; y })
    in
    if has_focus then Render.renderer#draw_txr target txr_hover ~position () ;
    super#draw target

end

class screen items actionnables = object(self)

  val mutable selected = None

  method private select actionnable =
    selected >? (fun o -> o#set_focus false) ;
    selected <- Some actionnable ;
    actionnable#set_focus true

  method private focus_held =
    match selected with
      | None -> false
      | Some o -> o#holds_focus

  method draw (target : OgamlGraphics.Window.t) =

    List.iter (fun i -> i#draw target) items ;
    List.iter (fun a -> a#draw target) actionnables

  method handle_key key =
    if self#focus_held then selected >? (fun o -> o#handle_key key)
    else key |>
    OgamlCore.Keycode.(function
      | Left -> self#left
      | Right -> self#right
      | Up -> self#up
      | Down -> self#down
      | Space | Return -> self#action
      | Escape -> Manager.manager#pop
      | _ -> ()
    )

  method private action =
    selected >? fun o -> (Sounds.play_sound "enter" ; o#action)

  method private sqdist a b =
    let x = a#x -. b#x
    and y = a#y -. b#y in
    int_of_float (x *. x +. y *. y)

  method private weight horizontal s a =
    if horizontal then
      self#sqdist a s * (abs (int_of_float (a#y -. s#y)) + 1)
    else
      self#sqdist a s * (abs (int_of_float (a#x -. s#x)) + 1)

  method private compare h s a b =
    (self#weight h s a - self#weight h s b)

  method private move p h = selected >? fun s ->
    Sounds.play_sound "click";
    List.find_all (fun a -> a <> s && p a s) actionnables
    |> List.sort (self#compare h s)
    |> function
        | a :: _ -> self#select a
        | _ -> ()

  method private left = self#move (fun a s -> a#x < s#x) true
  method private right = self#move (fun a s -> a#x > s#x) true
  method private up = self#move (fun a s -> a#y < s#y) false
  method private down = self#move (fun a s -> a#y > s#y) false

  initializer
    match actionnables with
    | a :: _ -> self#select a
    | _ -> ()

end
