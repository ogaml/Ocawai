open OgamlGraphics
open OgamlMath
open OgamlUtils

type t = 
  {
    fbo1 : Framebuffer.t;
    fbo2 : Framebuffer.t;
    rt1 : Texture.Texture2D.t;
    rt2 : Texture.Texture2D.t;
    size : Vector2i.t;
    blur_program : Program.t;
    filter_program : Program.t;
    additive_program : Program.t;
    fullrect : VertexArray.SimpleVertex.T.s VertexArray.Source.t;
  }

let create (type s) (module M : RenderTarget.T with type t = s) ctx size = 
  let fbo1,fbo2,rt1,rt2 = 
    Framebuffer.create (module M) ctx,
    Framebuffer.create (module M) ctx,
    Texture.Texture2D.create (module M) ctx (`Empty size),
    Texture.Texture2D.create (module M) ctx (`Empty size)
  in
  Framebuffer.attach_color (module Texture.Texture2D) fbo1 0 rt1;
  Framebuffer.attach_color (module Texture.Texture2D) fbo2 0 rt2;
  let blur_program = 
    Program.from_source_pp (module M) ~context:ctx ~log:Log.stdout
      ~vertex_source:(`File "resources/glsl/bloom.vsh")
      ~fragment_source:(`File "resources/glsl/blur.fsh") ()
  in
  let filter_program = 
    Program.from_source_pp (module M) ~context:ctx ~log:Log.stdout
      ~vertex_source:(`File "resources/glsl/bloom.vsh")
      ~fragment_source:(`File "resources/glsl/filter.fsh") ()
  in
  let additive_program = 
    Program.from_source_pp (module M) ~context:ctx ~log:Log.stdout
      ~vertex_source:(`File "resources/glsl/bloom.vsh")
      ~fragment_source:(`File "resources/glsl/add.fsh") ()
  in
  let fullrect =
    VertexArray.(Source.(
      empty ~size:6 () 
        << SimpleVertex.create 
          ~position:Vector3f.({x = -1.0; y = -1.0; z = 0.0})
          ~uv:Vector2f.({x = 0.0; y = 0.0}) ()
        << SimpleVertex.create 
          ~position:Vector3f.({x =  1.0; y = -1.0; z = 0.0})
          ~uv:Vector2f.({x = 1.0; y = 0.0}) ()
        << SimpleVertex.create 
          ~position:Vector3f.({x =  1.0; y =  1.0; z = 0.0})
          ~uv:Vector2f.({x = 1.0; y = 1.0}) ()
        << SimpleVertex.create 
          ~position:Vector3f.({x = -1.0; y = -1.0; z = 0.0})
          ~uv:Vector2f.({x = 0.0; y = 0.0}) ()
        << SimpleVertex.create 
          ~position:Vector3f.({x =  1.0; y =  1.0; z = 0.0})
          ~uv:Vector2f.({x = 1.0; y = 1.0}) ()
        << SimpleVertex.create 
          ~position:Vector3f.({x = -1.0; y =  1.0; z = 0.0})
          ~uv:Vector2f.({x = 0.0; y = 1.0}) ()
    ))
  in
  {
    fbo1; fbo2; rt1; rt2; size;
    blur_program;
    filter_program;
    additive_program;
    fullrect
  }

let filter (type s) (module M : RenderTarget.T with type t = s) ctx 
  (sys:t) (source : Texture.Texture2D.t) = 
  Framebuffer.clear sys.fbo1;
  let vbo = VertexArray.Buffer.static (module M) ctx sys.fullrect in
  let vao = VertexArray.create (module M) ctx [VertexArray.Buffer.unpack vbo] in
  let parameters = 
    DrawParameter.(make
      ~culling:CullingMode.CullNone
      ~blend_mode:BlendMode.alpha
      ~depth_write:false
      ~depth_test:DepthTest.None) ()
  in
  let uniform = 
    Uniform.empty
    |> Uniform.vector2f "trg_size" (Vector2f.from_int (Framebuffer.size sys.fbo1))
    |> Uniform.texture2D "src_tex" source
  in
  VertexArray.draw (module Framebuffer) 
    ~target:sys.fbo1
    ~vertices:vao
    ~program:sys.filter_program
    ~parameters
    ~uniform
    ()

let blur (type s) (module M : RenderTarget.T with type t = s) ctx (sys:t) radius =
  Framebuffer.clear sys.fbo2;
  let vbo = VertexArray.Buffer.static (module M) ctx sys.fullrect in
  let vao = VertexArray.create (module M) ctx [VertexArray.Buffer.unpack vbo] in
  let parameters = 
    DrawParameter.(make
      ~culling:CullingMode.CullNone
      ~blend_mode:BlendMode.alpha
      ~depth_write:false
      ~depth_test:DepthTest.None) ()
  in
  let uniform = 
    Uniform.empty
    |> Uniform.vector2f "trg_size" (Vector2f.from_int (Framebuffer.size sys.fbo1))
    |> Uniform.float "radius" radius
  in
  let uniform1 = 
    uniform
    |> Uniform.texture2D "src_tex" sys.rt1
    |> Uniform.vector2f "dir" (Vector2f.unit_x)
  in
  VertexArray.draw (module Framebuffer) 
    ~target:sys.fbo2
    ~vertices:vao
    ~program:sys.blur_program
    ~parameters
    ~uniform:uniform1
    ();
  Framebuffer.clear sys.fbo1;
  let uniform2 = 
    uniform
    |> Uniform.texture2D "src_tex" sys.rt2
    |> Uniform.vector2f "dir" (Vector2f.unit_y)
  in
  VertexArray.draw (module Framebuffer) 
    ~target:sys.fbo1
    ~vertices:vao
    ~program:sys.blur_program
    ~parameters
    ~uniform:uniform2
    ()

let add (type s) (module M : RenderTarget.T with type t = s) (ctx:s)
        (sys:t) (source : Texture.Texture2D.t) (target:s) =
  let vbo = VertexArray.Buffer.static (module M) ctx sys.fullrect in
  let vao = VertexArray.create (module M) ctx [VertexArray.Buffer.unpack vbo] in
  let parameters = 
    DrawParameter.(make
      ~culling:CullingMode.CullNone
      ~blend_mode:BlendMode.alpha
      ~depth_write:false
      ~depth_test:DepthTest.None) ()
  in
  let uniform = 
    Uniform.empty
    |> Uniform.vector2f "trg_size" (Vector2f.from_int (M.size target))
    |> Uniform.texture2D "src_tex" source
    |> Uniform.texture2D "blurred_tex" sys.rt1
  in
  VertexArray.draw (module M) 
    ~target
    ~vertices:vao
    ~program:sys.additive_program
    ~parameters
    ~uniform:uniform
    ()

let blooming (type s) (module M : RenderTarget.T with type t = s) (ctx : s)
             (sys:t) (source : Texture.Texture2D.t) (target : s) =
  filter (module M) ctx sys source;
  blur (module M) ctx sys 4.;
  blur (module M) ctx sys 2.;
  blur (module M) ctx sys 1.;
  add (module M) ctx sys source target
  
