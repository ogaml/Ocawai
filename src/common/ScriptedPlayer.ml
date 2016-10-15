open OgamlUtils
open Player

let log_str output = `Fun(function
  |`String(s) -> Printf.fprintf output "%s" s; `Unit
  | _ -> assert false
)

let log_int output = `Fun(function
  |`Int(i) -> Printf.fprintf output "%i" i; `Unit
  | _ -> assert false
)

class scripted_player ?(id) (scr : string)  =

  let () = Log.info Log.stdout "Creating player" in

  object (self)

  inherit player ?id:id ()

  val mutable logicPlayerList = []

  val mutable script = Interpreter.empty_script ()

  val mutable output = stdout

  val mutable playable_buildings = []

  initializer
    Log.info Log.stdout "Initializing player";
    Log.info Log.stdout "Getting player buildings";
    playable_buildings <- self#get_buildings;
    Log.info Log.stdout "Creating player log";
    let logname = Printf.sprintf "log/player_%i.log" self#get_id in
    Log.info Log.stdout "Opening log";
    output <- open_out logname;
    Log.info Log.stdout "Done"

  method init_script map players =
    script <- ScriptEngine.script_from_file scr
      [("self", `Player(self :> Player.logicPlayer));
       ("players", (`List(List.map (fun p -> `Player(p)) players)));
       ("map", (`Map(map)));
       ("log_int", log_int output);
       ("log_str", log_str output)]
      [("self", `Player_t);
       ("players", `List_t(`Player_t));
       ("map", `Map_t);
       ("selected_unit", `Soldier_t);
       ("selected_building", `Building_t);
       ("selected_pos", `Pair_t(`Int_t, `Int_t));
       ("log_int", `Fun_t(`Int_t, `Unit_t));
       ("log_str", `Fun_t(`String_t, `Unit_t))]

  method private get_next_movement =
    try
      let u = Interpreter.main_script script in
      let m = Interpreter.move_script script u in
      try
        let t = Interpreter.attack_script script m u in
        (m, Action.Attack_unit(u#get_id,t#get_id))
      with
      | ScriptCore.Do_nothing -> (m, Action.Wait)
    with
    | ScriptCore.End_turn -> ([], Action.End_turn)
    | Invalid_argument(s) ->
        print_endline ("Warning, script invalid argument : " ^ s);
        ([], Action.End_turn)

  method private get_next_build =
    try
      let b = List.hd playable_buildings in
      playable_buildings <- List.tl playable_buildings;
      let u = Interpreter.building_script script b in
      ([], Action.Create_unit (b#get_id,u))
    with
    | ScriptCore.Do_nothing -> self#get_next_action
    | ScriptCore.End_turn -> ([], Action.End_turn)
    | Invalid_argument(s) ->
        print_endline ("Warning, script invalid argument : " ^ s);
        ([], Action.End_turn)

  method get_next_action =
    Thread.delay 0.10;
    if self#has_playable_unit then
      self#get_next_movement
    else if playable_buildings <> [] then
      self#get_next_build 
    else
      ([], Action.End_turn)


  method update (u:Types.update) =
    match u with
    |Types.Your_turn ->
        Interpreter.init_script script;
        playable_buildings <- self#get_buildings
    | _ -> ()


end
