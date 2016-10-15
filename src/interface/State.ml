class virtual state = object(self)

  method virtual render : OgamlGraphics.Window.t -> unit

  method handle_event (e : OgamlCore.Event.t) = ()

  method resumed = ()

  method paused = ()

  method destroy = ()

end
