open OgamlGraphics

type alignment = Center | Left | Right
type quantity = Pix of int | Pt of float

let to_pixels = function
  | Pix i -> i
  | Pt p -> int_of_float (0.72 *. p)

let rect_print (type s) (module M : RenderTarget.T with type t = s)(target : s) =
  let rec print_aux string font color size interline alignment rectangle =

    let character_size = to_pixels size in
    let interline_size = to_pixels interline in

    let text = ref (Text.create
      ~text:string ~font ~color ~size:(character_size) ~bold:false
      ~position:(OgamlMath.FloatRect.position rectangle) ()
    ) in
    let getstring = ref string in
    let getposition = ref (OgamlMath.FloatRect.position rectangle) in

    let setstring string =
      text := Text.create
        ~text:string ~font ~color ~size:(character_size) ~bold:false
        ~position:!getposition () ;
      getstring := string
    in

    let setposition position =
      text := Text.create
        ~text:!getstring ~font ~color ~size:(character_size) ~bold:false
        ~position:!getposition () ;
      getposition := position
    in

    let text_bounds = Text.boundaries !text in

    try

      (* If we cannot draw it in height, we're done *)
      if OgamlMath.FloatRect.(text_bounds.height > rectangle.height)
      then raise Exit ;

      (* If the text is too long for a line *)
      if OgamlMath.FloatRect.(text_bounds.width > rectangle.width) then begin

        (* We compute the line to be drawn and draw the rest recursively *)
        let words = Str.split (Str.regexp " ") !getstring in

        (* We clear the text object *)
        setstring "" ;

        (* We create a function to check if s is too long *)
        (* Sets the text if not too long and if [reset] is false *)
        let set_too_long reset s =
          let old_s = !getstring in
          setstring s ;
          let text_bounds = Text.boundaries !text in
          let b = OgamlMath.FloatRect.(text_bounds.width > rectangle.width) in
          if (reset || b) then setstring old_s ; b
        in
        (* Pure comparison *)
        let toolong = set_too_long true in

        (* Enlarges the string while it fits *)
        let rec fit strel sep =
          match strel with
          | [] -> ""
          | e :: r ->
              let s = !getstring in
              if not (set_too_long false (s ^ sep ^ e)) then
                fit r sep
              else String.concat sep strel
        in

        (* We have to treat the case where the first word is already too long *)
        let char_list str =
          let l = ref [] in
          String.iter (fun c -> l := (String.make 1 c) :: !l) str;
          List.rev !l
        in

        (* Computes the remaining text and sets the current line *)
        let remaining = match words with
          | [] -> failwith "inconsistent text"
          | w::r when toolong w -> String.concat " " ((fit (char_list w) "") :: r)
          | strel -> fit strel " "
        in

        (* Dealing with the interline *)
        let delta = float_of_int (character_size + interline_size) in
        let new_rect = OgamlMath.FloatRect.({
          x = rectangle.x ;
          y = rectangle.y +. delta ;
          width = rectangle.width ;
          height = rectangle.height -. delta
        }) in

        (* We are now ready to call the function recursively *)
        print_aux remaining font color size interline alignment new_rect
      end ;

      (* In case things were modified we recompute the bounds *)
      let text_bounds = Text.boundaries !text in

      (* The line we're about to draw is aligned through offset *)
      let ox = match alignment with
        | Center -> OgamlMath.FloatRect.(rectangle.width -. text_bounds.width)/.2.
        | Left   -> 0.
        | Right  -> OgamlMath.FloatRect.(rectangle.width -. text_bounds.width)
      in

      (* Poor attempt at vertical centering *)
      let oy =
        (float_of_int interline_size) -.
        OgamlMath.FloatRect.(text_bounds.height) /. 4.
      in

      setposition OgamlMath.Vector2f.({
        x = OgamlMath.FloatRect.(rectangle.x) +. ox ;
        y = OgamlMath.FloatRect.(rectangle.y) +. oy
      }) ;

      Text.draw (module M) ~text:!text ~target ()

    with Exit -> ()
  in
  print_aux
