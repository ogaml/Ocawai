open Position
open Utils

exception GeneratorFailure

exception NotEnoughSpawns
exception BadSpawnsSequence
exception NotEnoughPlace
exception InvalidPositioning
exception UnitsSuperposition
exception NoPath
exception UnitsSpawnFail
exception StructSpawnFail

(* special tiles used by the generation *)
let seed_tile = Tile.create_from_config "seed"
let blank_tile = Tile.create_from_config "blank"
let is_blank m p = Tile.get_name (Battlefield.get_tile m p) = "blank"
let is_seed m p = Tile.get_name (Battlefield.get_tile m p) = "seed"

(* functions used to get the 4 or 8 direct neighbors of a position, 
  as a position list or a tile list *)

let neigh_list t = [up t;down t;left t;right t]
let neigh_list_corners t = [up t;down t;left t;right t;up (left t);down (right t);left (down t);right (up t)]

let pos_neighbors_aux m f p =
  List.filter
    (Battlefield.in_range m)
    (f p)

let pos_neighbors_corners m p =
  pos_neighbors_aux m neigh_list_corners p

let pos_neighbors m p =
  pos_neighbors_aux m neigh_list p

let neighbors_corners m pos =
  List.map
    (Battlefield.get_tile m)
    (pos_neighbors_corners m pos)

let neighbors m pos =
  List.map
    (Battlefield.get_tile m)
    (pos_neighbors m pos)

(* function used to get the neighbors of a position list not already in the entry list, and without duplications *) 

module SetPos = Set.Make(Position)

let rec neighbours_set_aux f = function
| [] -> SetPos.empty
| p::q -> List.fold_right SetPos.add (f p) (neighbours_set_aux f q)

let neighbours_set l =
  SetPos.filter
    (fun e -> not (List.mem e l))
    (neighbours_set_aux neigh_list l)

let neighbours_corners_set l =
  SetPos.filter
    (fun e -> not (List.mem e l))
    (neighbours_set_aux neigh_list_corners l)

let neighbours l =
  SetPos.fold 
    (fun e l -> e::l) 
    (neighbours_set l)
    []

let neighbours_corners l =
  SetPos.fold 
    (fun e l -> e::l) 
    (neighbours_corners_set l)
    []

(* general utility functions *)

let count f l = List.fold_left (fun c e -> if f e then 1+c else c) 0 l

let matrix_init sx sy f =
  Array.init
    sx
    (fun x ->
      Array.init
      sy
      (f x)
    )

let matrix_foreach f =
  Array.iteri (fun x -> Array.iteri (f x))


(* return the list of list of positions of connected composants among positions
   that satifies f *)
let find_connected_composants f m =
  let sizex, sizey = Battlefield.size m in
  (* singletons *)
  let sings =
    matrix_init
      sizex
      sizey
      (fun x y ->
        let p = Position.create (x, y) in
        if f p then
          Some(UnionFind.make_sing p)
        else
          None
      ) in
  let union a (x',y') =
    sings.(x').(y') >? (UnionFind.union_gen a) in
  (* union when neighbours are walkables *)
  matrix_foreach
    (fun x y aopt ->
      aopt >? (fun a ->
        List.iter
        (fun b ->
          if Battlefield.in_range m b then
            let x', y' = topair b in
            union a (x', y')
        )
        (neighbours [Position.create (x,y)]))
    )
    sings;
  List.map
    UnionFind.get_data
    (
      List.map
        (function Some(r) -> r | None -> assert false)
      (List.filter
        (function
          | None -> false
          | Some(r) -> UnionFind.is_representative r)
        (List.concat (Array.to_list (Array.map Array.to_list sings))))
    )
  
  


(* functions working with densities *)
(* compute the total density of a list of tiles *)
let total_density tiles =
  List.fold_left (+) 0 (List.map Tile.get_density tiles)

(* return a random tile with regards to densities *)
let get_tile_with_density total tiles =
  let d = (Random.int total) + 1 in
  let rec nth_dens n = function
  | p::q when Tile.get_density p < n ->
      nth_dens (n - Tile.get_density p) q
  | p::q -> p
  | _ -> assert false
  in
  nth_dens d tiles

(* the contiguity of a position is defined as its number of identical neighbors on its total nmber of neighbors *)
let contiguity m pos =
  let name = Tile.get_name (Battlefield.get_tile m pos) in
  let nei = neighbors_corners m pos in
  let a = count (fun t -> Tile.get_name t = name) nei in
  let b = List.length nei in
  float_of_int a /. float_of_int b


(* smooth a map by swaping random tiles iff the swap increase the local contigity *)
let swap_smoothing m factor =
  let (width,height) = Battlefield.size m in
  let swap pos1 pos2 =
    let t1 = Battlefield.get_tile m pos1 in
    Battlefield.set_tile m pos1 (Battlefield.get_tile m pos2);
    Battlefield.set_tile m pos2 t1
  in
  for i = 0 to factor * width * height do
    let pos1 = create (Random.int width , Random.int height) in
    let pos2 = create (Random.int width , Random.int height) in
    let pc1 = contiguity m pos1 in
    let pc2 = contiguity m pos2 in 
    swap pos1 pos2;
    let c1 = contiguity m pos1 in
    let c2 = contiguity m pos2 in
    (* the swap is canceled if the contiguity has decreased 
      or if we are looking at a seed tile 
      (they can be rendered to examine how seed_gen works, we don't want to move them ) *)
    if pc1 +. pc2 > c1 +. c2  || is_seed m pos1 || is_seed m pos2 then
      swap pos1 pos2;
  done

(* remove isolated tiles (isolated is defined by contiguity <= range) 
  by changing them to their most occuring neighbor *)
let hard_smoothing m tiles range =
  (* to be changed position list *)
  let to_smooth = 
    Battlefield.tile_filteri 
    (fun p t -> 
      (not (is_seed m p))
      && contiguity m p <= range
    )
    m
  in
  List.iter
    (fun pos ->
      (* count neighbors to find the most occuring one, and set it at pos *)
      let nei = neighbors_corners m pos in
      let nb = Array.make (List.length tiles) 0 in
      let rec find_n e = function
      | [],_ -> assert false
      | p::q,n when Tile.get_name p = Tile.get_name e -> n
      | p::q,n -> find_n e (q,n+1)
      in
      let rec count_nb = function
      | [] -> ()
      | p::q -> 
        let n = find_n p (tiles,0) in 
        nb.(n) <- nb.(n) + 1; 
        count_nb q
      in
      count_nb nei;
      let ma = ref (-1) in
      let ma_n = ref (-1) in
      for i = 0 to List.length tiles - 1 do
        if !ma < nb.(i) then (ma := nb.(i);ma_n:=i)
      done;
      let t = List.nth tiles !ma_n in
      Battlefield.set_tile m pos t
    )
    to_smooth

(* generate a map randomly then uses swap_smoothing *)
let swap_gen width height =
  let tiles_all = Tile.create_list_from_config () in
  let tiles =
    List.filter
    (fun a -> Tile.get_structure a = `Block)
    tiles_all in
  if (List.length tiles = 0) then failwith("no `Block tiles in config") else
  let total = total_density tiles in
  let m = Battlefield.create width height (List.hd tiles) in

  (* fill the map with regards to densities *)
  Battlefield.tile_iteri
    (fun p _ -> Battlefield.set_tile m p (get_tile_with_density total tiles))
    m;
  swap_smoothing m 50;
  (* change tiles with less than two identical neighbors *)
  hard_smoothing m tiles (2./.8.);
  m

(* place seeds (generation centers) and expand them until the whole map is filled *)
let seeds_gen width height =
  let nbseeds = width*height/50 in
  let tiles_all = Tile.create_list_from_config () in
  let tiles =
    List.filter
    (fun a -> Tile.get_structure a = `Block)
    tiles_all in
  if (List.length tiles = 0) then failwith("no `Block tiles in config") else
  let m = Battlefield.create width height blank_tile in
  let total = total_density tiles in
  (* initial seeds creation *)
  let rec create_seeds = function
  | 0 -> []
  | n ->
    let t = get_tile_with_density total tiles in
    let p = create(Random.int width,Random.int height) in
    if List.for_all 
        (fun pos -> 
          Battlefield.in_range m pos
          && ( is_blank m pos 
              || Tile.get_name (Battlefield.get_tile m p) = Tile.get_name t
             )
        )
        (let ne = pos_neighbors_corners m p in p::ne@(neighbours_corners ne)) 
    then
    (
      Battlefield.set_tile m p t; (* option : replace t by seed_tile in order to visualize the seeds on the map *)
      (t,p)::(create_seeds (n-1))
    )
    else
      create_seeds (n-1)
  in
  (* grows seed t on a position list *)
  let rec grow_list m t = function
  | [] -> ()
  | p::q ->
    if is_blank m p then
      Battlefield.set_tile m p t;
    grow_list m t q
  in
  (* extract at most n elements of a list*)
  let rec behead = function
  | 0,_ -> []
  | n,_ when n<0 -> assert false
  | n,[] -> []
  | n,p::q -> p::(behead (n-1,q))
  in
  (* grow seed t on nb_grow tiles of its neighbors position set setpos*)
  let grow_seed t nb_grow setpos =
    let sl = behead (nb_grow,Utils.shuffle (SetPos.fold (fun e l -> e::l) setpos [])) in
    grow_list m t sl;
    SetPos.filter
      (fun p -> is_blank m p)
      (SetPos.union 
        (SetPos.filter 
          (fun p -> Battlefield.in_range m p)
          (neighbours_set sl)
        )
        setpos
      )
  in
  (* grow every seed simultaneously, remove the ones that can't be expanded, until no seed can grow*)
  let rec grow = function
  | [] -> ()
  | ll -> 
    grow (
        List.filter 
          (fun (t,l) -> l <> SetPos.empty )
          (
            List.map 
            (fun (t,s) -> 
              (
                t, grow_seed t 
                  (max 
                    1 
                    (Tile.get_grow_speed t)
                  ) s
              )
            )
            ll
          )
        )
  in
  (* initial seeds *)
  let seeds = create_seeds nbseeds in
  (* associates their neighbors position set to them *)
  let neigh = 
    List.map 
      (fun (t,p) -> 
        (t, SetPos.filter 
              (fun pos -> 
                Battlefield.in_range m pos
                && is_blank m pos
              )
              (neighbours_set [p])
        )
      )
      seeds
  in
  grow neigh;
  (* change tiles with less than two identical neighbors *)
  hard_smoothing m tiles (2./.8.);
  m
  

(* armies are in valid positions only if their units are placed on tiles where they can move *)
let check_movement attempt =
  let (m,a,_) = attempt in
  let b =
    List.for_all
      (List.for_all
        (fun u -> Tile.traversable (Battlefield.get_tile m (u#position))
        u)
      )
      a in
  if not b then
    raise InvalidPositioning

(* test connexity between armies *)
let check_path attempt =
  let (m,_,sp) = attempt in
  let sp1 = List.hd sp in
  let dij = Path.dijkstra m sp1 Unit.Tread in
  let b =
    List.for_all
      (fun sp2 -> dij sp2 <> None )
      (List.tl sp) in
  if not b then
    raise NoPath

(* test that there is at most one unit per position *)
let check_superposed_units attempt = let (m,a,_) = attempt in
  let (w,h) = Battlefield.size m in
  let t = Array.make_matrix w h false in
  List.iter
    (fun l ->
      List.iter
        (fun u ->
          let (x,y) = topair u#position in
          if t.(x).(y) then
            raise UnitsSuperposition
          else
            t.(x).(y) <- true
        )
        l
    )
    a

(* head of the positioning function, separated to avoid calculing it multiple times if positioning fails *)
let init_positioning m nbplayers =
  let (width,height) = Battlefield.size m in
  let poslist = ref ([]:Position.t list) in
  (* test if pos is far enough for the sides of the map (10% margin) *)
  let test_dist_edge pos =
    let (a,b) = topair(pos) in
    a > 10*width/100
    && b > 10*height/100
    && a < 90*width/100
    && b < 90*height/100
  in
  (* find possibles armies central points, the spawns, represented by their general.
    A position is considered valid iff it is the center of a 3*3 plain square *)
  Battlefield.tile_iteri
    (
      let is_plain t = Tile.get_name t = "plain" in
      fun pos ti ->
        let nei = neighbors_corners m pos in
        if test_dist_edge pos
            && Tile.get_name ti = "plain"
            && count is_plain nei = 8 then
          poslist := pos:: !poslist
    )
    m;
  if List.length !poslist < nbplayers then
    raise NotEnoughSpawns
  else
    !poslist

(* positions nbplayers armies on a map m, with legit_spawns the list returned by init_positioning *)
let positioning m nbplayers legit_spawns =
  let (width,height) = Battlefield.size m in
  let rec behead = function
  | 0,_ -> []
  | n,[] -> raise BadSpawnsSequence
  | n,p::q -> p::(behead (n-1,q))
  in
  (* test if a given position is far enough for all the previously selected positions.
    Far enough means that the distance between the two positions is at least 90% of the map size, on the number of player. *)
  let rec test_dist_spawns p = function
    | [] -> true
    | p1::q ->
        (Position.dist p p1 > (90*width + 90*height)/(100*nbplayers))
        && test_dist_spawns p q
  in
  (* extract a random valid sequence of spawns *)
  let filtered_pos = ref [] in
  List.iter
    (
      fun pos ->
        if test_dist_spawns pos !filtered_pos then
          filtered_pos := pos :: !filtered_pos
    )
    (Utils.shuffle legit_spawns);
  (* check if there are enough of them (more than nbplayers) *)
  let poslist = behead (nbplayers, Utils.shuffle !filtered_pos) in
  (* check the existence of a path between all armies *)
  check_path (m,(),poslist);

  (* place an army around the position spawn, knowing the other armies positions (to avoid overlaps on small maps)*)
  let position_army_around spawn other_armies_pos =
    let unbound_list = Unit.create_list_from_config() in
    let army = ref [Unit.bind (Unit.create_from_config "general") spawn] in
    let army_pos = ref [spawn] in
    List.iter
      (
        fun ui ->
          for i = 0 to ui#spawn_number - 1 do
            let ne = List.filter
              (
                fun p ->
                  Battlefield.in_range m p
                  && (Tile.traversable_m (Battlefield.get_tile m p) ui#movement_type)
                  && not (List.mem p other_armies_pos)
              )
              (neighbours !army_pos) in
            if ne = [] then
              raise NotEnoughPlace
            else
              (
                let r = Random.int (List.length ne) in
                let pos = List.nth ne r in
                army := (Unit.bind ui pos) :: !army;
                army_pos := pos :: !army_pos;
              )
          done;
      )
      unbound_list;
    (!army, (!army_pos)@other_armies_pos)
  in
  (* iter position_army_around for all armies*)
  let rec position_armies = function
  | 0 -> (([]:Unit.t list list),([]:Position.t list))
  | n when n > 0 ->
      let others = position_armies (n-1) in
      let ap = position_army_around (List.nth poslist (n-1)) (snd others) in
      ((fst ap)::(fst others),snd ap)
  | _ -> assert false
  in
  (fst (position_armies nbplayers), poslist)


(* create roads and bridges on a map*)
let create_roads m = () (*TODO*)


(* create beaches and other (?) borders of `Block *)
let create_borders m =
  let borders = Tile.create_list_from_config () in
  (* create the border (water : string, rate : int (0-1000), expansion : int) composed of beach tiles *)
  let create_border (water, rate, expansion) beach =
    let poslist_water =
      Battlefield.tile_filteri 
        (fun pos t ->
          Tile.get_name t = water
          && count (fun u -> Tile.get_name u = water) (neighbors_corners m pos) <> 8)
        m 
      in
    let poslist_beach =
      Utils.shuffle (List.filter 
          (fun pos -> 
            let t = Battlefield.get_tile m pos in 
            Tile.get_name t <> water 
            && match Tile.compare_walkability t beach with | None -> false | Some i -> i = 0)
          (List.filter 
            (Battlefield.in_range m) 
            (neighbours_corners poslist_water)
          ) )
    in
    (* isolates a proportion of possible beach seeds*)
    let rec behead = function
    | 0,_ -> []
    | n,[] -> assert false
    | n,p::q -> p::(behead (n-1,q))
    in
    let seeds_beach = behead (rate*(List.length poslist_beach)/1000, poslist_beach ) in
    (* grow beaches around those seeds*)
    let grow_border m seed =
      let bord = ref [seed] in
      for i = 1 to expansion do
        bord := !bord @ (List.filter 
                          (fun pos -> 
                            Battlefield.in_range m pos
                            && List.mem pos poslist_beach)
                          (neighbours_corners !bord));
      done;
      !bord
    in
    List.iter 
      (fun pos -> Battlefield.set_tile m pos beach) 
      (List.fold_left 
        (fun l e -> (grow_border m e) @ l) 
        [] seeds_beach
      )
  in
  (* iterates over all `Border tiles *)
  List.iter 
    (fun beach -> 
      match Tile.get_structure beach with 
      | `Border param -> create_border param beach
      | _ -> ()
    )
    borders

(* create structures on a map *)
let create_structs m =
  create_borders m;
  create_roads m

(* iterated tries to spawn armies *)
let units_spawn m nbplayers nbattempts legit_spawns =
  let rec units_spawn_aux = function
  | 0 -> raise UnitsSpawnFail
  | n ->
    begin
      print_string ("    attempt "^(string_of_int (nbattempts - n +1))^" / "^(string_of_int nbattempts)^": ");
      try
        let (a,sp) = positioning m nbplayers legit_spawns in
        let attempt = (m,a,sp) in
        print_string "armies spawned, checking... ";
        flush_all();
        check_movement attempt;
        check_superposed_units attempt;
        check_path attempt; (* place here any checks on units positioning*)
        print_endline "success";
        (a,sp)
      with
      | BadSpawnsSequence ->
          print_endline "Not enough spawns found"; 
          units_spawn_aux (n-1)
      | NotEnoughPlace ->
          print_endline " Not enough space around spawn for army";
          units_spawn_aux (n-1)
      | InvalidPositioning ->
          print_endline "Unit placed on an area not coresponding to its movement modes";
          units_spawn_aux (n-1)
      | UnitsSuperposition ->
          print_endline "Units superposition";
          units_spawn_aux (n-1)
      | NoPath ->
          print_endline "No path between armies";
          units_spawn_aux (n-1)
    end
  in
  print_endline "  Spawning armies ...";
  units_spawn_aux nbattempts

(* iterated tries to create structures *)
let create_structures m nbplayers nbattempts =
  let rec create_structures_aux = function
  | 0 -> raise StructSpawnFail
  | n ->
    begin
      print_string ("    attempt "^(string_of_int (nbattempts - n +1))^" / "^(string_of_int nbattempts)^": ");
      try
        create_structs m;
        print_endline "structures spawn success"
        (* place here any checks on structures positioning*)
      with
      | StructSpawnFail ->
          print_newline();
          raise StructSpawnFail
    end
  in
  print_endline "  Spawning structures ...";
  create_structures_aux nbattempts

(* iterated tries to generate the map *)
let generate width height nbplayers nbattempts1 nbattempts2 nbattempts3 =
  let rec generate_aux = function
  | 0 -> 
    print_endline("generator failed, not enough tries? bad calling arguments?");
    raise GeneratorFailure
  | n ->
    begin
      print_endline ("  attempt "^(string_of_int (nbattempts1 - n +1))^" / "^(string_of_int nbattempts1)^": ");
      try
        let m = seeds_gen width height in
        create_structures m nbplayers nbattempts2;
        let (a,sp) = units_spawn m nbplayers nbattempts3 (init_positioning m nbplayers) in
        let attempt = (m,a,sp) in
        print_endline "Generation success"(* place here any check on map generation*);
        attempt
      with
      | StructSpawnFail ->
          print_endline "  structures spawn aborted";
          generate_aux (n-1)
      | NotEnoughSpawns ->
          print_endline "  Spawning armies ...\n   not enough valid spawns\n  armies spawn aborted";
          generate_aux (n-1)
      | UnitsSpawnFail ->
          print_endline "  armies spawn aborted";
          generate_aux (n-1)
    end
  in
  print_endline "Generating Battlefield : ";
  generate_aux nbattempts1


class t (width:int) (height:int) (nbplayers:int) (generate_attempts:int) (*(structs_attempts:int)*) (units_spawn_attempts:int)=
object (self)
  val g = Random.self_init();generate width height nbplayers generate_attempts (*structs_attempts*) 1 units_spawn_attempts
  method field = let m,_,_ = g in m
  method armies = let _,a,_ = g in a
  method spawns = let _,_,sp = g in sp
end

