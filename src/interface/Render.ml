open OgamlGraphics
open OgamlMath
open OgamlUtils
open Utils
open Tileset

let renderer = object(self)

  val texture_library = TextureLibrary.create ()

  val tileset_library = TilesetLibrary.create ()

  val font = Font.load (Utils.base_path () ^ "fonts/FreeSansBold.ttf")

  val mutable rect_source = VertexArray.Source.empty () 

  val mutable program = Obj.magic ()

  method init win =
    let folder = (Utils.base_path ()) ^ "textures/" in
    TextureLibrary.load_directory win texture_library (folder);
    TilesetLibrary.load_directory win tileset_library (folder);
    program <- 
      Program.from_source_pp 
        (module Window) 
        ~context:win
        ~log:Log.stdout
        ~vertex_source:(`File "resources/glsl/default.vsh")
        ~fragment_source:(`File "resources/glsl/default.fsh") ()


  (* Returns true iff p is in the map *)
  method private filter_positions map p =
    not (Position.out_of_bounds p
        (Position.create (0,0))
        (Position.diff
          (Position.create (Battlefield.size map))
          (Position.create (1,1))))

  (* Draw a tile from a set, using a VAO *)
  method private draw_tile set tilename
    ?position:(position = Vector2f.zero)
    ?scale:(scale = Vector2f.({x = 1.; y = 1.}))
    ?color:(color = `RGB Color.RGB.white)
    ?origin:(origin = Vector2f.zero) () =
    let open Vector2f in
    let source = set#source in
    let tex_size = float_of_int set#tile_size in
    let new_origin = {x = scale.x *. origin.x; y = scale.y *. origin.y} in
    let real_pos = Vector2f.sub position new_origin in
    let real_end = Vector2f.prop tex_size scale in
    let texture_rect = set#texture_rect tilename in
    let open VertexArray in
    let open Source in
    source
    << (SimpleVertex.create
      ~position:(Vector3f.lift real_pos)
      ~color
      ~uv:Vector2f.({x = texture_rect.xmin; y = texture_rect.ymin}) ())
    << (SimpleVertex.create
      ~position:(Vector3f.lift ({x = real_pos.x +. real_end.x; y = real_pos.y}))
      ~color
      ~uv:Vector2f.({x = texture_rect.xmax; y = texture_rect.ymin}) ())
    << (SimpleVertex.create
      ~position:(Vector3f.lift (Vector2f.add real_pos real_end))
      ~color
      ~uv:Vector2f.({x = texture_rect.xmax; y = texture_rect.ymax}) ())
    << (SimpleVertex.create
      ~position:(Vector3f.lift real_pos)
      ~color
      ~uv:Vector2f.({x = texture_rect.xmin; y = texture_rect.ymin}) ())
    << (SimpleVertex.create
      ~position:(Vector3f.lift (Vector2f.add real_pos real_end))
      ~color
      ~uv:Vector2f.({x = texture_rect.xmax; y = texture_rect.ymax}) ())
    << (SimpleVertex.create
      ~position:(Vector3f.lift ({x = real_pos.x; y = real_pos.y +. real_end.y}))
      ~color
      ~uv:Vector2f.({x = texture_rect.xmin; y = texture_rect.ymax}) ())
    |> ignore

  method draw_direct_tile (target : Window.t) (set : Tileset.tileset)
    tilename ?position ?rotation ?scale
    ?color ?origin () : unit =
    let texture_rect = set#int_rect tilename in
    let spr = 
      Sprite.create 
        ~texture:set#texture 
        ?position 
        ?rotation
        ?scale 
        ?color
        ?origin 
        ~subrect:texture_rect () 
    in
    Sprite.draw (module Window) ~target ~sprite:spr ()

  (* Draw a texture *)
  method draw_txr (target : Window.t) name
    ?position
    ?rotation
    ?scale:(scale = Vector2f.({x = 1.; y = 1.}))
    ?size
    ?color
    ?centered:(centered = true)
    ?blend_mode:(blend_mode = DrawParameter.BlendMode.alpha) () : unit =
    let texture = TextureLibrary.get_texture texture_library name in
    let tsize =  Vector2f.from_int (Texture.Texture2D.size texture) in
    let origin  =
      if centered then Vector2f.prop 0.5 tsize
      else Vector2f.zero
    in
    let scale   =
      match size with
      |None -> scale
      |Some(s) ->
        let scalem  = Vector2f.({x = s.x /. tsize.x; y = s.y /. tsize.y}) in
        Vector2f.({x = scale.x *. scalem.x; y = scale.y *. scalem.y})
    in
    let sprite = 
      Sprite.create
        ~origin 
        ?position 
        ?rotation 
        ?color
        ~texture 
        ~scale 
        ()
    in
    let parameters = 
      DrawParameter.(make 
        ~culling:CullingMode.CullNone
        ~depth_test:DepthTest.None
        ~depth_write:false
        ~blend_mode
        ()
      )
    in
    Sprite.draw (module Window) ~target ~sprite ~parameters ()


  (* Draw a sprite from a map position and an offset *)
  method private draw_from_map
    (target : Window.t) camera name position
    ?rotation
    ?offset:(offset = Vector2f.zero)
    ?scale:(scale = Vector2f.({x = 1.; y = 1.}))
    ?blend_mode
    ?color () : unit =
    let (x,y) = camera#project position in
    let position = 
      Vector2f.add
        (Vector2f.from_int Vector2i.({x;y}))
        (Vector2f.prop camera#zoom offset)
    in
    let scale = 
      Vector2f.prop camera#zoom scale
    in 
    self#draw_txr target name ~position ?color
      ?rotation ~scale ?blend_mode ()

  (* Draw a tile from a map position and an offset *)
  method private draw_tile_from_map camera set name position
    ?offset:(offset = Vector2f.zero)
    ?scale:(scale = Vector2f.({x = 1.; y = 1.}))
    ?color () : unit =
    let (x,y) = camera#project position in
    let position = 
      Vector2f.add
        (Vector2f.from_int Vector2i.({x;y}))
        (Vector2f.prop camera#zoom offset)
    in
    let o = float_of_int set#tile_size /. 2. in
    let scale = 
      Vector2f.prop camera#zoom scale
    in 
    self#draw_tile set name ~position ?color ~scale ~origin:Vector2f.({x = o; y = o}) ()

  (* Render the joints *)
  method private render_joints camera jointset pos texture_name map =
    (* Utility *)
    let draw_v = self#draw_tile_from_map camera jointset ~offset:Vector2f.({x = 0.; y = -2.}) in
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
        draw_h ~offset:Vector2f.({x = 2.; y = 0.}) "water_ground_hr" left ()
      else if is_ground texture_name && is_water leftname then
        draw_h ~offset:Vector2f.({x = -2.; y = 0.}) "water_ground_h" pos ()
      else if is_beach texture_name && is_ground leftname then
        draw_h ~offset:Vector2f.({x = 2.; y = 0.}) "beach_ground_hr" left ()
      else if is_ground texture_name && is_beach leftname then
        draw_h ~offset:Vector2f.({x = -2.; y = 0.}) "beach_ground_h" pos ()
      else if is_water texture_name && is_beach leftname then
        draw_h ~offset:Vector2f.({x = 2.; y = 0.}) "water_beach_hr" left ()
      else if is_beach texture_name && is_water leftname then
        draw_h ~offset:Vector2f.({x = -2.; y = 0.}) "water_beach_h" pos ()
    end

  (* Highlight a tile *)
  method private highlight_tile (target : Window.t)
    camera base_color pos =
    let (r,g,b,a) = Color.RGB.(
      base_color.r, base_color.g, base_color.b,
      base_color.a) in
    let multiplier = sin (Unix.gettimeofday () *. 2.) +. 2. in
    let alpha = ((multiplier /. 3.) *. a) in
    self#draw_from_map target camera "highlight" pos
      ~color:(`RGB Color.RGB.({r; g; b; a = alpha}))
      ~blend_mode:DrawParameter.BlendMode.additive ()

  (* Render the whole map (also draws the tile VAO) *)
  method private render_map (target : Window.t)
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
    let parameters = 
      DrawParameter.(make
        ~culling:CullingMode.CullNone
        ~depth_test:DepthTest.None
        ~depth_write:false
        ~blend_mode:BlendMode.alpha ()
      )
    in
    let vbo =
      VertexArray.Buffer.static (module Window) target tileset#source
    in
    let vao = 
      VertexArray.create (module Window) target [VertexArray.Buffer.unpack vbo]
    in
    let uniform =
      Uniform.empty
      |> Uniform.vector2f "win_size" (Vector2f.from_int (Window.size target))
      |> Uniform.vector2f "tex_size" (Vector2f.from_int (Texture.Texture2D.size tileset#texture))
      |> Uniform.texture2D "mtexture" tileset#texture
    in
    VertexArray.draw (module Window) 
      ~target 
      ~vertices:vao 
      ~program 
      ~parameters
      ~uniform 
      ~mode:DrawMode.Triangles ();
    VertexArray.Source.clear tileset#source;
    let vbo = 
      VertexArray.Buffer.static (module Window) target jointset#source
    in
    let vao = 
      VertexArray.create (module Window) target [VertexArray.Buffer.unpack vbo]
    in
    let uniform =
      Uniform.empty
      |> Uniform.vector2f "win_size" (Vector2f.from_int (Window.size target))
      |> Uniform.vector2f "tex_size" (Vector2f.from_int (Texture.Texture2D.size jointset#texture))
      |> Uniform.texture2D "mtexture" jointset#texture
    in
    VertexArray.draw (module Window) 
      ~target 
      ~vertices:vao 
      ~program 
      ~parameters
      ~uniform
      ~mode:DrawMode.Triangles ();
    VertexArray.Source.clear jointset#source

  (* Render a path with arrows *)
  method private draw_path (target : Window.t)
    camera path =
    let draw = self#draw_from_map target camera in
    let angle s t =
      match Position.diff t s with
        | pos when pos = Position.create (1,0)  -> 0.
        | pos when pos = Position.create (0,1)  -> Constants.pi2
        | pos when pos = Position.create (-1,0) -> Constants.pi
        | pos when pos = Position.create (0,-1) -> -. Constants.pi2
        | _ -> failwith "Not continuous path"
    in
    let rec aux prev = function
      | pos :: [] -> draw "arrow_end" pos ~rotation:(angle prev pos) ()
      | pos :: next :: r ->
        let ap = angle pos prev
        and an = angle pos next in
        let (amax,amin) = if an > ap then (an,ap) else (ap,an) in
        if amax = amin +. Constants.pi then
          draw "arrow_straight" pos ~rotation:ap ()
        else begin
          if amax = amin +. (3. *. Constants.pi2) then
            draw "arrow_corner" pos ~rotation:(3. *. Constants.pi2) ()
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
  (target : Window.t) (uphandle : Updates.handler) camera foggy character my_unit =
    let (u_position,(ox,oy)) = uphandle#unit_position my_unit in
    let offset = Vector2f.({x = ox; y = oy}) in
    if foggy u_position then ()
    else begin
      let color =
        if my_unit#has_played
        then `RGB Color.RGB.({r = 0.6; g = 0.6; b = 0.6; a = 1.0}) 
        else `RGB Color.RGB.white
      in
      let name = character ^ "_" ^ my_unit#name in
      self#draw_from_map ~offset target camera name u_position ~color ();
      let size = int_of_float (camera#zoom *. 14.) in
      let (x,y) = camera#project u_position in
      let position = 
        Vector2f.add
          (Vector2f.from_int Vector2i.({x;y}))
          (Vector2f.prop camera#zoom offset)
      in
      let text = 
        Text.create 
          ~text:(string_of_int (my_unit#hp))
          ~position 
          ~font 
          ~color:(`RGB Color.RGB.({r = 0.9; g = 0.9; b = 0.95; a = 1.0})) 
          ~size 
          ~bold:false ()
      in
      Text.draw (module Window) ~target ~text ()
    end

  (* Draw a building *)
  method private draw_building 
  (target : Window.t) camera character building =
    let name = character ^ "_" ^ building#name in
    self#draw_from_map target camera name (building#position) ()

  (* Render a range (move or attack, according to cursor's state) *)
  method private draw_range (target : Window.t) camera map =
    match camera#cursor#get_state with
    | Cursor.Idle -> ()
    | Cursor.Displace(_,my_unit,(range,_)) -> begin
      let attack_range = Position.range camera#cursor#position my_unit#min_attack_range
        my_unit#attack_range in
      let attack_range =
        List.filter (self#filter_positions map) attack_range
      in
      List.iter (self#highlight_tile target camera
        (Color.RGB.({r = 1.0; g = 1.0; b = 0.4; a = 0.6})))  range;
      if List.mem camera#cursor#position range then
        List.iter (self#highlight_tile target camera
          (Color.RGB.({r = 1.0; g = 0.2; b = 0.2; a = 1.0}))) attack_range
    end
    | Cursor.Action(my_unit,p,_) -> begin
      let range = Position.range p my_unit#min_attack_range
        my_unit#attack_range in
      let attack_range =
        List.filter (self#filter_positions map) range
      in
      List.iter (self#highlight_tile target camera
          (Color.RGB.({r = 1.0; g = 0.2; b = 0.2; a = 1.0}))) attack_range
    end
    | Cursor.Build _ | Cursor.Watched_attack -> ()

  (* Draw the cursor *)
  method private draw_cursor (target : Window.t)
    (camera : Camera.camera) =
    let texname =
      Cursor.(match camera#cursor#get_state with
      | Idle | Displace(_,_,_) | Build _ -> "cursor"
      | Action(_,_,_) | Watched_attack -> "sight")
    in
    let (x,y) = camera#cursor#offset in
    let f = camera#cursor#scale in
    self#draw_from_map target camera texname camera#cursor#position
      ~offset:(Vector2f.({x = -.x; y = -.y}))
      ~scale:(Vector2f.({x = f; y = f})) ()

  method draw_gui (target : Window.t)
    (ui_manager : UIManager.ui_manager) =
    ui_manager#draw target texture_library

  (* Draw the whole game *)
  method render_game (target : Window.t)
    (data : ClientData.client_data) (uphandle : Updates.handler) =

    let sizef = Vector2f.from_int (Window.size target) in

    let sprite = 
      Sprite.create 
        ~texture:(TextureLibrary.(get_texture texture_library "background"))
        ~scale:Vector2f.({x = (Pervasives.max (sizef.x /. 2048.) 1.); 
                          y =  Pervasives.max (sizef.y /. 2048.) 1.})
        ()
    in
    Sprite.draw (module Window) ~target ~sprite ();
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
    data#minimap#draw (module Window) target data#camera#cursor;
    (* Displaying case information *)
    let drawer s pos =
      self#draw_txr target s 
        ~position:(Vector2f.({x = fst pos; y = snd pos}))
        ~size:(Vector2f.({x = 30.; y = 30.})) ()
    in
    let tile_drawer s pos =
      let set = TilesetLibrary.get_tileset tileset_library "tileset" in
      self#draw_direct_tile target set s
        ~position:(Vector2f.({x = fst pos; y = snd pos}))
        ~scale:(Vector2f.({x = 30. /. 50.; y = 30. /. 50.})) ()
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
      (module Window)
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
    GuiTools.(rect_print (module Window)
      target (resources ^ " flowers") font (`RGB Color.RGB.white) (Pix 30) (Pix 10) Right
      FloatRect.({ x = 20. ; y = 5. ; width = sizef.Vector2f.x -. 40. ; height = 100. })); 
    (* Display players turn *)
    let current = Updates.(match uphandle#current_turn with
      | Your_turn -> "Your turn"
      | Turn_of id -> Printf.sprintf "Turn of #%d" id
      | Nobody_s_turn -> ""
    ) in
    GuiTools.(rect_print (module Window)
      target current font (`RGB Color.RGB.white) (Pix 30) (Pix 10) Right
      FloatRect.({ x = 20. ; y = sizef.Vector2f.y -. 50. ; width = sizef.Vector2f.x -. 40. ; height = 100. }));
    (* Display speed of action *)
    let speed = uphandle#speed in
    if speed <> "normal" then
      self#draw_txr target speed 
        ~position:Vector2f.({x = sizef.x-.50.; y = sizef.y-.70.}) 
        ~size:Vector2f.({x = 75.; y = 75.}) () ;
    (* Display end of game *)
    uphandle#end_screen target ;
    (* Display framerate *)
    FPS.display (module Window) target

end
