open OgamlGraphics

type t

val create : (int * int) -> t

val blooming : (module RenderTarget.T with type t = 'a) -> t -> Window.t -> 'a -> unit
