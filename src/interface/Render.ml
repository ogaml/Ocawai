open OgamlGraphics
open OgamlMath
open OgamlUtils
open Utils
open Tileset

let renderer = object(self)

  val texture_library = TextureLibrary.create ()

  val tileset_library = TilesetLibrary.create ()

  val font = Font.load (Utils.base_path () ^ "fonts/FreeSansBold.ttf")

  val mutable rect_source = VertexArray.VertexSource.empty () 

  method init win =
    let folder = (Utils.base_path ()) ^ "textures/" in
    TextureLibrary.load_directory win texture_library (folder);
    TilesetLibrary.load_directory win tileset_library (folder)

  (* Returns true iff p is in the map *)
  method private filter_positions map p =
    not (Position.out_of_bounds p
        (Position.create (0,0))
        (Position.diff
          (Position.create (Battlefield.size map))
          (Position.create (1,1))))

  (* Draw a tile from a set, using a VAO *)
  method private draw_tile set tilename
    ?position:(position = (0.,0.))
    ?scale:(scale = 1.,1.)
    ?color:(color = Color.rgb 255 255 255)
    ?origin:(origin = 0.,0.) () =
    let vao = set#vao in
    let tex_size = float_of_int set#tile_size in
    let new_origin = (fst scale *. fst origin, snd scale *. snd origin) in
    let real_pos = Utils.subf2D position new_origin in
    let real_end = (fst scale *. tex_size, snd scale *. tex_size) in
    let texture_rect = set#texture_rect tilename in
    vao#append (mk_vertex
      ~position:real_pos
      ~color
      ~tex_coords:(texture_rect.xmin, texture_rect.ymin) ());
    vao#append (mk_vertex
      ~position:(addf2D real_pos (fst real_end, 0.))
      ~color
      ~tex_coords:(texture_rect.xmax, texture_rect.ymin) ());
    vao#append (mk_vertex
      ~position:(addf2D real_pos real_end)
      ~color
      ~tex_coords:(texture_rect.xmax, texture_rect.ymax) ());
    vao#append (mk_vertex
      ~position:(addf2D real_pos (0., snd real_end))
      ~color
      ~tex_coords:(texture_rect.xmin, texture_rect.ymax) ())

  method draw_direct_tile (target : render_window) (set : Tileset.tileset)
    tilename ?position ?rotation ?scale
    ?color ?origin () =
    let texture_rect = set#int_rect tilename in
    let spr = new sprite ~texture:set#texture ?position ?rotation
      ?scale ?color ?origin ~texture_rect () in
    target#draw spr

  (* Draw a texture *)
  method draw_txr (target : render_window) name
    ?position
    ?rotation
    ?scale:(scale = (1.,1.))
    ?size
    ?color
    ?centered:(centered = true)
    ?blend_mode () =
    let texture = TextureLibrary.get_texture texture_library name in
    let (sx,sy) =  foi2D texture#get_size in
    let origin  =
      if centered then (sx/.2.,sy/.2.)
      else (0.,0.)
    in
    let scale   =
      match size with
      |None -> scale
      |Some(s) ->
        let scalem  = (fst s /. sx, snd s /. sy) in
        (fst scale *. fst scalem, snd scale *. snd scalem)
    in
    new sprite ~origin ?position ?rotation ~texture ~scale ?color ()
    |> target#draw ?blend_mode


  (* Draw a sprite from a map position and an offset *)
  method private draw_from_map
    (target : render_window) camera name position
    ?rotation
    ?offset:(offset = (0.,0.))
    ?scale:(scale = (1.,1.))
    ?blend_mode
    ?color () =
    let (ox,oy) = offset in
    let position = addf2D
      (foi2D (camera#project position))
      (ox *. camera#zoom, oy *. camera#zoom)
    in
    let scale = (fst scale *. camera#zoom, snd scale *. camera#zoom) in
    self#draw_txr target name ~position ?color
      ?rotation ~scale ?blend_mode ()

  (* Draw a tile from a map position and an offset *)
  method private draw_tile_from_map camera set name position
    ?offset:(offset = (0.,0.))
    ?scale:(scale = (1.,1.))
    ?color () =
    let position = addf2D
      (foi2D (camera#project position))
      ((fst offset) *. camera#zoom, (snd offset) *. camera#zoom)
    in
    let o = float_of_int set#tile_size /. 2. in
    let scale = (fst scale *. camera#zoom, snd scale *. camera#zoom) in
    self#draw_tile set name ~position ?color ~scale ~origin:(o,o) ()

  (* Render the joints *)
  method private render_joints camera jointset pos texture_name map =
    (* Utility *)
    let draw_v = self#draw_tile_from_map camera jointset ~offset:(0.,-2.) in
    let draw_h = self#draw_tile_from_map camera jointset in
    (* Hardcode for testing *)
    (* Let's draw the junction *)
    let up = Position.up pos in
    let left = Position.left pos in
    let is_ground name =
      name = "plain" || name = "forest" || name = "mountain"
    in
    let is_water name =
      name = "water" || name = "lake"
    in
    let is_beach name =
      name = "beach" || name = "lake_beach" || name = "port_beach"
    in
    if self#filter_positions map up then
    begin
      let upname = Tile.get_name (Battlefield.get_tile map up) in
      if is_water texture_name && is_ground upname then
        draw_v "ground_water_v" pos ()
      else if is_ground texture_name && is_water upname then
        draw_v "water_ground_v" pos ()
      else if is_beach texture_name && is_ground upname then
        draw_v "ground_beach_v" pos ()
      else if is_ground texture_name && is_beach upname then
        draw_v "beach_ground_v" pos ()
      else if is_water texture_name && is_beach upname then
        draw_v "beach_water_v" pos ()
      else if is_beach texture_name && is_water upname then
        draw_v "water_beach_v" pos ()
    end;
    if self#filter_positions map left then
    begin
      let leftname = Tile.get_name (Battlefield.get_tile map left) in
      if is_water texture_name && is_ground leftname then
        draw_h ~offset:(2.,0.) "water_ground_hr" left ()
      else if is_ground texture_name && is_water leftname then
        draw_h ~offset:(-2.,0.) "water_ground_h" pos ()
      else if is_beach texture_name && is_ground leftname then
        draw_h ~offset:(2.,0.) "beach_ground_hr" left ()
      else if is_ground texture_name && is_beach leftname then
        draw_h ~offset:(-2.,0.) "beach_ground_h" pos ()
      else if is_water texture_name && is_beach leftname then
        draw_h ~offset:(2.,0.) "water_beach_hr" left ()
      else if is_beach texture_name && is_water leftname then
        draw_h ~offset:(-2.,0.) "water_beach_h" pos ()
    end

  (* Highlight a tile *)
  method private highlight_tile (target : render_window)
    camera base_color pos =
    let (r,g,b,a) = Color.(
      base_color.r, base_color.g, base_color.b,
      float_of_int (base_color.a)) in
    let multiplier = sin (Unix.gettimeofday () *. 2.) +. 2. in
    let alpha = int_of_float ((multiplier /. 3.) *. a) in
    self#draw_from_map target camera "highlight" pos
      ~color:(Color.rgba r g b alpha)
      ~blend_mode:BlendAdd ()

  (* Render the whole map (also draws the tile VAO) *)
  method private render_map (target : render_window)
    camera (map : Battlefield.t) =
    let tileset = TilesetLibrary.get_tileset tileset_library "tileset" in
    let jointset = TilesetLibrary.get_tileset tileset_library "tilejoints" in
    let render_tile tile_name p =
      self#draw_tile_from_map camera tileset tile_name p ();
      self#render_joints camera jointset p tile_name map
    in
    List.iter
      (fun p -> render_tile (Tile.get_name (Battlefield.get_tile map p)) p)
      (Position.square camera#top_left camera#bottom_right);
    target#draw tileset#vao ~texture:tileset#texture;
    tileset#vao#clear;
    target#draw jointset#vao ~texture:jointset#texture;
    jointset#vao#clear

  (* Render a path with arrows *)
  method private draw_path (target : render_window)
    camera path =
    let draw = self#draw_from_map target camera in
    let angle s t =
      match Position.diff t s with
        | pos when pos = Position.create (1,0)  -> 0.
        | pos when pos = Position.create (0,1)  -> 90.
        | pos when pos = Position.create (-1,0) -> 180.
        | pos when pos = Position.create (0,-1) -> -90.
        | _ -> failwith "Not continuous path"
    in
    let rec aux prev = function
      | pos :: [] -> draw "arrow_end" pos ~rotation:(angle prev pos) ()
      | pos :: next :: r ->
        let ap = angle pos prev
        and an = angle pos next in
        let (amax,amin) = if an > ap then (an,ap) else (ap,an) in
        if amax = amin +. 180. then
          draw "arrow_straight" pos ~rotation:ap ()
        else begin
          if amax = amin +. 270. then
            draw "arrow_corner" pos ~rotation:270. ()
          else
            draw "arrow_corner" pos ~rotation:amax ()
        end ;
        aux pos (next :: r)
      | [] -> ()
    in
    match path with
      | start :: [] -> draw "arrow_lone" start ()
      | start :: next :: r ->
        draw "arrow_start" start ~rotation:(angle start next) ();
        aux start (next :: r)
      | [] -> ()

  (* Render a unit *)
  method private draw_unit
  (target : render_window) uphandle camera foggy character my_unit =
    let (u_position,offset) = uphandle#unit_position my_unit in
    if foggy u_position then ()
    else begin
      let color =
        if my_unit#has_played
        then Color.rgb 150 150 150
        else Color.rgb 255 255 255
      in
      let name = character ^ "_" ^ my_unit#name in
      self#draw_from_map ~offset target camera name u_position ~color ();
      let size = int_of_float (camera#zoom *. 14.) in
      let (ox,oy) = offset in
      let position = addf2D
        (foi2D (camera#project u_position))
        (ox *. camera#zoom, oy *. camera#zoom)
      in
      new text ~string:(string_of_int (my_unit#hp))
        ~position ~font ~color:(Color.rgb 230 230 240) ~character_size:size ()
      |> target#draw
    end

  (* Draw a building *)
  method private draw_building
  (target : render_window) camera character building =
    let name = character ^ "_" ^ building#name in
    self#draw_from_map target camera name (building#position) ()

  (* Render a range (move or attack, according to cursor's state) *)
  method private draw_range (target : render_window) camera map =
    match camera#cursor#get_state with
    | Cursor.Idle -> ()
    | Cursor.Displace(_,my_unit,(range,_)) -> begin
      let attack_range = Position.range camera#cursor#position my_unit#min_attack_range
        my_unit#attack_range in
      let attack_range =
        List.filter (self#filter_positions map) attack_range
      in
      List.iter (self#highlight_tile target camera
        (Color.rgba 255 255 100 150)) range;
      if List.mem camera#cursor#position range then
        List.iter (self#highlight_tile target camera
          (Color.rgba 255 50 50 255)) attack_range
    end
    | Cursor.Action(my_unit,p,_) -> begin
      let range = Position.range p my_unit#min_attack_range
        my_unit#attack_range in
      let attack_range =
        List.filter (self#filter_positions map) range
      in
      List.iter (self#highlight_tile target camera
        (Color.rgba 255 50 50 255)) attack_range
    end
    | Cursor.Build _ | Cursor.Watched_attack -> ()

  (* Draw the cursor *)
  method private draw_cursor (target : render_window)
    (camera : Camera.camera) =
    let texname =
      Cursor.(match camera#cursor#get_state with
      | Idle | Displace(_,_,_) | Build _ -> "cursor"
      | Action(_,_,_) | Watched_attack -> "sight")
    in
    self#draw_from_map target camera texname camera#cursor#position
      ~offset:(Utils.subf2D (0.,0.) camera#cursor#offset)
      ~scale:(camera#cursor#scale, camera#cursor#scale) ()

  (* TODO Draw the GUI *)
  (*method draw_gui (target : render_window)
    (ui_manager : UIManager.ui_manager) =
    ui_manager#draw target texture_library*)

  (* Draw the whole game *)
  method render_game (target : render_window)
    (data : ClientData.client_data) (uphandle : Updates.handler) =

    let (sx, sy) = Utils.foi2D target#get_size in

    new sprite
      ~texture:(TextureLibrary.(get_texture texture_library "background"))
      ~scale:(max (sx /. 2048.) 1., max (sy /. 2048.) 1.)
      ()
    |> target#draw;
    (* For the fog *)
    let fog = data#actual_player#get_fog in
    let foggy p = Fog.hidden fog p in
    (* Rendering *)
    self#render_map target data#camera data#map;
    self#draw_range target data#camera data#map;
    self#draw_path target data#camera data#current_move;
    self#draw_cursor target data#camera;
    uphandle#update;
    (* Draw buildings *)
    List.iter
      (self#draw_building target data#camera "neutral")
      data#neutral_buildings;
    List.iter (fun p ->
      let chara = Characters.to_string (Characters.handler#character_of p) in
      List.iter
        (self#draw_building target data#camera chara)
        p#get_buildings
    ) data#players;
    (* Draw units *)
    List.iter (fun p ->
      let chara = Characters.to_string (Characters.handler#character_of p) in
      List.iter
        (self#draw_unit target uphandle data#camera foggy chara)
        p#get_army
    ) data#players;
    (* Drawing an explosion *)
    begin match uphandle#burst_position with
    | Some pos ->
        self#draw_from_map target data#camera "boom" pos ()
    | None -> ()
    end ;
    (* Displaying fog *)
    let camera = data#camera in
    List.iter
      (fun p -> if (foggy p) then self#draw_from_map target camera "fog" p ())
      (Position.square camera#top_left camera#bottom_right);
    (* Displaying minimap *)
    data#minimap#draw target data#camera#cursor;
    (* Displaying case information *)
    let drawer s pos =
      self#draw_txr target s ?position:(Some pos) ?size:(Some (30.,30.)) ()
    in
    let tile_drawer s pos =
      let set = TilesetLibrary.get_tileset tileset_library "tileset" in
      self#draw_direct_tile target set s
        ~position:pos ~scale:(30./.50.,30./.50.) ()
    in
    let is_foggy = foggy data#camera#cursor#position in
    let s_unit =
      if is_foggy then None
      else data#unit_at_position data#camera#cursor#position
    in
    let chara = match s_unit with
    | Some selected_unit ->
        let player = data#player_of selected_unit in
        List.fold_left
            (fun a p ->
              let c =
                Characters.to_string (Characters.handler#character_of p)
              in
              if p = player then c else a
            )
            "" data#players
    | None -> ""
    in
    let damage = Cursor.(
        match (data#camera#cursor#get_state,s_unit) with
        | (Action(u1,_,_), Some u2) ->
            Some (Logics.damage_interval u1 u2)
        | _ -> None
      )
    in
    let (s_building, b_player) =
      data#building_at_position data#camera#cursor#position
    in
    let b_chara = match b_player with
    | Some player ->
        List.fold_left
          (fun a p ->
            let c = Characters.to_string (Characters.handler#character_of p) in
            if p = player then c else a
          )
          "" data#players
    | None -> "neutral"
    in
    let s_tile =
      Battlefield.get_tile data#map data#camera#cursor#position
    in
    data#case_info#draw
      target
      drawer
      tile_drawer
      damage
      is_foggy
      s_unit
      chara
      s_building
      b_chara
      s_tile ;
    (* Display resources *)
    let resources = string_of_int data#actual_player#get_value_resource in
    let (w,h) = foi2D target#get_size in
    GuiTools.(rect_print
      target (resources ^ " flowers") font Color.white (Pix 30) (Pix 10) Right
      { left = 20. ; top = 5. ; width = w -. 40. ; height = 100. });
    (* Display players turn *)
    let current = Updates.(match uphandle#current_turn with
      | Your_turn -> "Your turn"
      | Turn_of id -> Printf.sprintf "Turn of #%d" id
      | Nobody_s_turn -> ""
    ) in
    GuiTools.(rect_print
      target current font Color.white (Pix 30) (Pix 10) Right
      { left = 20. ; top = h -. 50. ; width = w -. 40. ; height = 100. });
    (* Display speed of action *)
    let speed = uphandle#speed in
    if speed <> "normal" then
      self#draw_txr target speed ~position:(w-.50.,h-.70.) ~size:(75.,75.) () ;
    (* Display end of game *)
    uphandle#end_screen target ;
    (* Display framerate *)
    FPS.display target

end
