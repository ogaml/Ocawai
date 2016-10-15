open OgamlGraphics
open OgamlMath

let timer = ref (Unix.gettimeofday ())

let framecount = ref 0

let current_fps = ref 0

let font = Font.load "resources/fonts/digit.ttf"

let display (type s) (module M : RenderTarget.T with type t = s) (target : s) =
  incr framecount;
  if Unix.gettimeofday () -. !timer >= 1. then begin
    timer := Unix.gettimeofday ();
    current_fps := !framecount;
    framecount := 0
  end;
  let text = 
    Text.create 
     ~text:(string_of_int !current_fps)
     ~font
     ~size:42
     ~position:Vector2f.zero
     ~bold:false
     ~color:(`RGB Color.RGB.blue) ()
  in
  Text.draw (module M) ~target ~text ()


