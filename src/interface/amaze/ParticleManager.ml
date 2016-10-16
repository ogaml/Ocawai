open OgamlGraphics
open OgamlMath

class base_particle ~position ~rotation ~speed 
  ~scale ~color ~life = object(self)

  val mutable position : (float * float) = position
  val mutable rotation = rotation
  val mutable speed    : (float * float) = speed
  val mutable scale    : float = scale
  val mutable color    : Color.t = color
  val mutable updates  = []

  val mutable dead = false

  val mutable age = 0.

  initializer
    self#add_update (fun dt -> 
      age <- age +. dt;
      if age > life then self#kill
    );
    self#add_update (fun dt ->
      let spe = (dt *. (fst speed), dt *. (snd speed)) in
      position <- Utils.addf2D position spe)

  method private origin = (scale *. 11., 1.)

  method update dt = List.iter (fun f -> f dt) updates

  method life_ratio = age /. life

  method add_update f = updates <- f :: updates

  method private kill = dead <- true

  method is_alive = not dead

  method render (rect_tex : Texture.Texture2D.t) (target : Framebuffer.t) = 
    let o = self#origin in
    let sprite = 
      Sprite.create 
        ~position:Vector2f.({x = fst position; y = snd position})
        ~scale:Vector2f.({x = scale; y = 1.})
        ~rotation
        ~texture:rect_tex
        ~color
        ~origin:Vector2f.({x = fst o; y = snd o}) ()
    in
    Sprite.draw (module Framebuffer) ~target ~sprite ()
end


class particle_manager (window : Window.t) = 

  let winsize = Window.size window in

  object(self)

  val mutable particles : base_particle list = []

  val mutable last_update : float = Unix.gettimeofday ()

  val bloomer = BloomEffect.create (module Window) window winsize

  val rectangle = Texture.Texture2D.create (module Window) window 
                      (`File "resources/textures/gui/rectangle.png")

  val pp_texture = Texture.Texture2D.create (module Window) window 
                      (`Empty winsize) 

  val pp_fbo = Framebuffer.create (module Window) window

  initializer
    Framebuffer.attach_color (module Texture.Texture2D) pp_fbo 0 pp_texture

  method add_particle : 'a. (#base_particle as 'a) -> unit = 
    fun p -> particles <- (p :> base_particle) :: particles

  method update = 
    let dt = Unix.gettimeofday () -. last_update in
    last_update <- last_update +. dt;
    particles <- List.filter (fun p -> p#update dt; p#is_alive) particles

  method render = 
    Framebuffer.clear pp_fbo;
    List.iter (fun (p : base_particle) -> p#render rectangle pp_fbo) particles;
    BloomEffect.blooming (module Window) window bloomer pp_texture window

end


    
