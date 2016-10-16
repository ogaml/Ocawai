open OgamlGraphics
open OgamlMath
open OgamlUtils

type mini_tile = Forest | Mountain | Plain | Water | Sand

type mini_unit = Unit | Building

let player_colors = Color.([|
  `RGB RGB.red ;
  `RGB RGB.green ;
  `RGB RGB.blue ;
  `RGB RGB.yellow
|])

let pairsum (a,b) = a + b

class minimap def width height = object(self)

  val map = Array.make_matrix def def Plain

  val player_map = Array.make_matrix def def None

  method private reset =
    for i = 0 to def - 1 do
      for j = 0 to def - 1 do
        player_map.(i).(j) <- None;
        map.(i).(j) <- Plain
      done;
    done

  method private compute_players players =
    let majority_map = Array.make_matrix def def [||] in
    let n = List.length players in
    for i = 0 to def - 1 do
      for j = 0 to def - 1 do
        majority_map.(i).(j) <- Array.make n (0,0)
      done;
    done;
    let act_player = ref 0 in
    List.iter (fun p ->
      List.iter (fun u ->
        let (px,py) = Position.topair u#position in
        let foi = float_of_int in
        let (px', py') = Utils.iof2D (
          ((foi px) /. (foi (width  - 1))) *. (foi (def - 1)),
          ((foi py) /. (foi (height - 1))) *. (foi (def - 1)))
        in
        majority_map.(px').(py').(!act_player) <-
          (fst majority_map.(px').(py').(!act_player) + 1, 0)
      ) p#get_army;
      incr act_player
    ) players;
    act_player := 0;
    List.iter (fun p ->
      List.iter (fun u ->
        let (px,py) = Position.topair u#position in
        let foi = float_of_int in
        let (px', py') = Utils.iof2D (
          ((foi px) /. (foi (width  - 1))) *. (foi (def - 1)),
          ((foi py) /. (foi (height - 1))) *. (foi (def - 1)))
        in
        majority_map.(px').(py').(!act_player) <-
          (fst majority_map.(px').(py').(!act_player),
           snd majority_map.(px').(py').(!act_player) + 1)
      ) p#get_buildings;
      incr act_player
    ) players;
    for i = 0 to def - 1 do
      for j = 0 to def - 1 do
        let maxp = ref (-1) in
        let maxu = ref 0    in
        for k = 0 to n-1 do
          if pairsum majority_map.(i).(j).(k) > !maxu then begin
            maxu := pairsum majority_map.(i).(j).(k);
            maxp := k
          end
        done;
        if !maxp <> -1 then
          let (a,b) = majority_map.(i).(j).(!maxp) in
          if a >= b then
            player_map.(i).(j) <- Some (!maxp, Unit)
          else
            player_map.(i).(j) <- Some (!maxp, Building)
      done;
    done

  method private compute_battlefield battlefield =
    let majority_map = Array.make_matrix def def [||] in
    for i = 0 to def - 1 do
      for j = 0 to def - 1 do
        majority_map.(i).(j) <- Array.make 5 0
      done;
    done;
    for i = 0 to width - 1 do
      for j = 0 to height - 1 do
        let foi = float_of_int in
        let (px, py) = Utils.iof2D (
          ((foi i) /. (foi (width  - 1))) *. (foi (def - 1)),
          ((foi j) /. (foi (height - 1))) *. (foi (def - 1)))
        in
        let pos = Position.create (i,j) in
        match Tile.get_name (Battlefield.get_tile battlefield pos) with
        | "forest" ->
            majority_map.(px).(py).(0) <-
              majority_map.(px).(py).(0) + 1
        | "mountain" ->
            majority_map.(px).(py).(1) <-
              majority_map.(px).(py).(1) + 1
        | "plain" ->
            majority_map.(px).(py).(2) <-
              majority_map.(px).(py).(2) + 1
        | "water" | "lake" | "shallow" ->
            majority_map.(px).(py).(3) <-
              majority_map.(px).(py).(3) + 1
        | "sand" | "beach" | "lake_beach" ->
            majority_map.(px).(py).(4) <-
              majority_map.(px).(py).(4) + 1
        | _ -> ()
      done;
    done;
    for i = 0 to def - 1 do
      for j = 0 to def - 1 do
        let maxt = ref (-1) in
        let maxn = ref (-1) in
        for k = 0 to 4 do
          if majority_map.(i).(j).(k) > !maxn then begin
            maxn := majority_map.(i).(j).(k);
            maxt := k
          end
        done;
        match !maxt with
        |0 -> map.(i).(j) <- Forest
        |1 -> map.(i).(j) <- Mountain
        |2 -> map.(i).(j) <- Plain
        |3 -> map.(i).(j) <- Water
        |4 -> map.(i).(j) <- Sand
        |_ -> assert false
      done;
    done

  method compute battlefield (players : Player.logicPlayer list) =
    self#reset;
    self#compute_battlefield battlefield;
    self#compute_players players

  method private add_rectangle vao pos size color =
    let open VertexArray in
    VertexSource.(
      vao << (SimpleVertex.create ~position:(Vector3f.lift pos) ~color ())
          << (SimpleVertex.create ~position:(Vector3f.lift Vector2f.(add pos {x = size.x; y = 0.})) ~color ())
          << (SimpleVertex.create ~position:(Vector3f.lift Vector2f.(add pos size)) ~color ())
          << (SimpleVertex.create ~position:(Vector3f.lift pos) ~color ())
          << (SimpleVertex.create ~position:(Vector3f.lift Vector2f.(add pos size)) ~color ())
          << (SimpleVertex.create ~position:(Vector3f.lift Vector2f.(add pos {x = 0.; y = size.y})) ~color ())
    )
    |> ignore

  val vao_source = VertexArray.VertexSource.empty ()

  val mutable program = None

  method draw : 'a. (module RenderTarget.T with type t = 'a) -> 'a ->
                Cursor.cursor -> unit =
    fun (type s) (module M : RenderTarget.T with type t = s)
        (target : s) cursor ->
    let foi = float_of_int in
    let ratio = 200. /. (foi def) in
    self#add_rectangle vao_source 
      Vector2f.({x = 4.; y = 4.}) 
      Vector2f.({x = 208.; y = 208.}) 
      (`RGB Color.RGB.{r = 0.8; g = 0.8; b = 0.8; a = 1.0});
    for i = 0 to def - 1 do
      for j = 0 to def - 1 do
        let fill_color =
          match map.(i).(j) with
          |Forest   -> `RGB Color.RGB.({r = 0.; g = 0.4; b = 0.; a = 1.0}) 
          |Mountain -> `RGB Color.RGB.({r = 0.2; g = 0.55; b = 0.; a = 1.0})
          |Plain    -> `RGB Color.RGB.({r = 0.3; g = 0.75; b = 0.3; a = 1.0})
          |Water    -> `RGB Color.RGB.({r = 0.2; g = 0.2; b = 0.9; a = 1.0})
          |Sand     -> `RGB Color.RGB.({r = 0.95; g = 0.95; b = 0.4; a = 1.0})
        in
        self#add_rectangle vao_source 
          Vector2f.({x = (8.+.(foi i)*.ratio); y = (8.+.(foi j)*.ratio)})
          Vector2f.({x = ratio; y = ratio}) fill_color;
        match player_map.(i).(j) with
        |None -> ()
        |Some(p,t) ->
          let alpha = (sin (Unix.gettimeofday () *. 3.) +. 1.)/. 2. in
          let alpha = (100. *. alpha) +. 50. in
          if t = Unit then
            self#add_rectangle vao_source 
              Vector2f.({x = (8.+.(foi i)*.ratio); y = (8.+.(foi j)*.ratio)})
              Vector2f.({x = ratio;y = ratio}) 
              (`RGB Color.RGB.({r = 1.0; g = 1.0; b = 1.0; a = alpha /. 255.}));
          self#add_rectangle vao_source 
            Vector2f.({x = (9.+.(foi i)*.ratio); y = (9.+.(foi j)*.ratio)})
            Vector2f.({x = (ratio-.2.); y =(ratio-.2.)}) player_colors.(p);
      done;
    done;
    let (px, py) = Position.topair cursor#position in
    let (px', py') = Utils.iof2D (
      ((foi px) /. (foi (width  - 1))) *. (foi (def - 1)),
      ((foi py) /. (foi (height - 1))) *. (foi (def - 1)))
    in
    self#add_rectangle vao_source 
      Vector2f.({x = (8.+.(foi px')*.ratio); y = (8.+.(foi py')*.ratio)})
      Vector2f.({x = ratio; y = ratio}) 
      (`RGB Color.RGB.({r = 1.0; g = 1.0; b = 1.0; a = 0.7}));
    let vao = 
      VertexArray.static (module M) target vao_source;
    in
    let program = 
      begin match program with
      | None   ->
        Program.from_source_pp (module M) ~context:target
          ~vertex_source:(`File "resources/glsl/minimap.vsh")
          ~fragment_source:(`File "resources/glsl/minimap.fsh")
          ~log:(OgamlUtils.Log.stdout)
          ()
      | Some p -> p
      end
    in
    let parameters = 
      DrawParameter.(make
        ~culling:CullingMode.CullNone
        ~blend_mode:BlendMode.alpha
        ~depth_write:false
        ~depth_test:DepthTest.None) ()
    in
    let uniform = 
      Uniform.empty
      |> Uniform.vector2f "trg_size" (Vector2f.from_int (M.size target))
    in
    VertexArray.draw (module M) 
      ~target
      ~vertices:vao
      ~program
      ~parameters
      ~uniform
      ();
    VertexArray.VertexSource.clear vao_source

end
