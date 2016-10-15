open OgamlGraphics
open OgamlUtils
open OgamlMath

exception Unknown_texture of string

type t = (string, Texture.Texture2D.t) Hashtbl.t

let create () = Hashtbl.create 13

let load_texture ctx lib path = 
  let i,i' = String.rindex path '.', String.rindex path '/' in 
  let name = String.sub path (i'+1) (i-i'-1) in 
  let ext  = String.sub path (i+1) (String.length path - i - 1) in
  if ext = "png" then begin 
    let tex = Texture.Texture2D.create (module Window) ctx (`File path) in
    Log.info Log.stdout "Texture : [stored] %s" name;
    Hashtbl.add lib name tex
  end


let rec load_recursively ctx lib prefix path =
  if Sys.is_directory (prefix ^ path) then begin
    let children = Sys.readdir (prefix ^ path ^ "/") in 
    Array.iter (load_recursively ctx lib (prefix ^ path ^ "/")) children
  end else
    load_texture ctx lib (prefix ^ path)


let load_directory ctx lib dir = 
  let children = Sys.readdir dir in 
  Array.iter (load_recursively ctx lib dir) children


let get_texture lib name = 
  try 
    Hashtbl.find lib name 
  with 
    |Not_found ->
      Log.info Log.stdout "Texture : Couldn't retrieve texture %s" name;
      raise (Unknown_texture name)

