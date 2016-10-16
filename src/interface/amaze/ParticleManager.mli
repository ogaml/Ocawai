class base_particle : position:(float*float) -> rotation:float -> 
  speed:(float*float) -> scale:float -> color:OgamlGraphics.Color.t ->
  life:float -> object

  val mutable position : float * float
  val mutable rotation : float
  val mutable speed    : float * float
  val mutable scale    : float
  val mutable color    : OgamlGraphics.Color.t
  
  method life_ratio : float
  
  method update : float -> unit

  method add_update : (float -> unit) -> unit

  method is_alive : bool

  method render : OgamlGraphics.Texture.Texture2D.t -> OgamlGraphics.Framebuffer.t ->
    unit

end


class particle_manager : OgamlGraphics.Window.t -> object

  method add_particle : #base_particle -> unit

  method update : unit

  method render : unit

end
