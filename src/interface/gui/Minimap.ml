open OgamlGraphics

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
    (* vao#append (mk_vertex ~position:pos ~color ());
    vao#append (mk_vertex ~position:(Utils.addf2D pos (fst size, 0.)) ~color ());
    vao#append (mk_vertex ~position:(Utils.addf2D pos size) ~color ());
    vao#append (mk_vertex ~position:(Utils.addf2D pos (0., snd size)) ~color ()) *)
    ()
    (* TODO *)

  (* val vao = new vertex_array ~primitive_type:Quads [] *)
  (* TODO *)

  method draw : 'a. (module RenderTarget.T with type t = 'a) -> 'a ->
                Cursor.cursor -> unit =
    fun (type s) (module M : RenderTarget.T with type t = s)
        (target : s) cursor ->
    let foi = float_of_int in
    let ratio = 200. /. (foi def) in
    (* self#add_rectangle vao (4.,4.) (208.,208.) (Color.rgb 200 200 200);
    for i = 0 to def - 1 do
      for j = 0 to def - 1 do
        let fill_color =
          match map.(i).(j) with
          |Forest   -> Color.rgb   0 100   0
          |Mountain -> Color.rgb  50 140   0
          |Plain    -> Color.rgb  80 180  80
          |Water    -> Color.rgb  50  50 220
          |Sand     -> Color.rgb 240 240  96
        in
        self#add_rectangle vao ((8.+.(foi i)*.ratio), (8.+.(foi j)*.ratio))
          (ratio,ratio) fill_color;
        match player_map.(i).(j) with
        |None -> ()
        |Some(p,t) ->
          let alpha = (sin (Unix.gettimeofday () *. 3.) +. 1.)/. 2. in
          let alpha = int_of_float (100. *. alpha) + 50 in
          if t = Unit then
            self#add_rectangle vao ((8.+.(foi i)*.ratio), (8.+.(foi j)*.ratio))
              (ratio,ratio) (Color.rgba 255 255 255 alpha);
          self#add_rectangle vao ((9.+.(foi i)*.ratio), (9.+.(foi j)*.ratio))
            (ratio-.2.,ratio-.2.) player_colors.(p);
      done;
    done;
    let (px, py) = Position.topair cursor#position in
    let (px', py') = Utils.iof2D (
      ((foi px) /. (foi (width  - 1))) *. (foi (def - 1)),
      ((foi py) /. (foi (height - 1))) *. (foi (def - 1)))
    in
    self#add_rectangle vao ((8.+.(foi px')*.ratio), (8.+.(foi py')*.ratio))
      (ratio, ratio) (Color.rgba 255 255 255 180);
    target#draw vao;
    vao#clear *)
    (* TODO *)
    ()

end
