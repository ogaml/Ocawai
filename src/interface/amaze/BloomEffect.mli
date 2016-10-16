open OgamlGraphics

type t

val create : (module RenderTarget.T with type t = 'a) -> 'a -> OgamlMath.Vector2i.t -> t

val blooming : (module RenderTarget.T with type t = 'a) -> 'a -> 
               t -> 
               Texture.Texture2D.t -> 
               'a -> unit
