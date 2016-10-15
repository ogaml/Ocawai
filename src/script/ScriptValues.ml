type value_type = [
  `Int_t    |
  `Unit_t   |
  `String_t |
  `Bool_t   |
  `Soldier_t|
  `Map_t    |
  `Player_t |
  `Alpha_t of int |
  `List_t  of value_type |
  `Array_t of value_type |
  `Fun_t   of value_type * value_type |
  `Pair_t  of value_type * value_type |
  `Building_t
]

type value = [
  `Int    of int              |
  `Unit                       |
  `String of string           |
  `Bool   of bool             |
  `List   of value list       |
  `Array  of value array      |
  `Fun    of (value -> value) |
  `Pair   of value * value    |
  `Soldier of Unit.t          |
  `Map     of Battlefield.t   |
  `Player  of Player.logicPlayer |
  `Building of Building.t
]

exception Script_value_not_found of string

let info fmt = OgamlUtils.(Log.info Log.stdout ("%s" ^^ fmt) "Script : ")
let error fmt = OgamlUtils.(Log.error Log.stdout ("%s" ^^ fmt) "Script : ")

let value_table = Hashtbl.create 13

let expose f t s =
  info "[exposed] %s" s;
  Hashtbl.replace value_table s (f,t)

let hide s =
  info "[hide] %s" s;
  Hashtbl.remove value_table s

let type_of s =
  try
    snd (Hashtbl.find value_table s)
  with
    | Not_found -> raise (Script_value_not_found s)

let value_of s =
  try
    fst (Hashtbl.find value_table s)
  with
    | Not_found -> raise (Script_value_not_found s)
