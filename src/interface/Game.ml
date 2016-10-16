
open OgamlGraphics
open OgamlUtils
open OgamlCore
open OgamlMath
open Utils

open Manager

open Player
open Menus

(* Type of selectable *)
type selectable = [
  | `Unit of Unit.t
  | `Building of Building.t
]


(* Relocating get_next_action outside ClientPlayer *)
let client_state = ref ClientPlayer.Idle

let set_client_state s =
  client_state := s

let get_next_action () =
  let rec get_aux () =
    match !client_state with
    | ClientPlayer.Received a -> client_state := ClientPlayer.Idle ; a
    | _ -> ([], Action.None_)
  in get_aux ()


let new_game ?character () =

  let m_cdata = new ClientData.client_data in

  let my_player =
    new ClientPlayer.client_player m_cdata#push_update get_next_action
  in

  let m_engine = new Game_engine.game_engine () in

  let (m_players, m_map) = m_engine#init_local (my_player :> player) 3 in

  let m_camera = 
    let wsize = Window.size manager#window in
    new Camera.camera
      ~def_tile_size:50
      ~w:wsize.Vector2i.x ~h:wsize.Vector2i.y
      ~initpos:(m_engine#cursor_init_position (my_player :> player)#get_id)
      ~maxpos:(Position.diff
        (Position.create (Battlefield.size m_map))
        (Position.create (1,1)))
  in

  let m_uphandle = new Updates.handler m_cdata m_camera in

  (* Distributing characters *)
  let () =
    let constraints = match character with
      | Some c -> [((my_player :> player)#get_id,c)]
      | None -> []
    in
    Characters.handler#init
      constraints
      (List.map (fun p -> p#get_id) m_engine#get_players)
  in

  object(self)

  inherit State.state as super

  val ui_manager = new UIManager.ui_manager

  val camera = m_camera

  val cdata : ClientData.client_data = m_cdata

  val uphandle = m_uphandle

  val atk_menu = new ingame_menu ~m_position:(0,0) ~m_width:150
    ~m_item_height:30 ~m_theme:Theme.red_theme
    ~m_bar_height:30 ~m_bar_icon:"menu_icon" ~m_bar_text:"Attack" ()

  val build_menu = new ingame_menu
    ~m_position:(0,0)
    ~m_width:290
    ~m_item_height:30 ~m_theme:Theme.yellow_theme
    ~m_bar_height:30 ~m_bar_icon:"menu_icon"
    ~m_bar_text:"Build" ()

  (* Last unit selected through X/W *)
  val mutable last_selected : selectable option = None

  (* Select next unit given a way to go *)
  method private select_playable lu lb =
    let rec list_after x = function
      | [] -> []
      | e :: r when x = e -> r
      | e :: r -> list_after x r
    in
    (* Selectable unit *)
    let oku u = not u#has_played in
    (* Selectable building *)
    let okb b =
      (* It can actually build things *)
      b#product <> []
      (* There is nobody occupying it *)
      && cdata#player_unit_at_position
          b#position
          cdata#actual_player
         = None
      (* The prices aren't too high *)
      && (
        let prices = List.map
          (fun s ->
            List.find
              (fun u -> u#name = s)
              Config.config#unbound_units_list
            |> (fun u -> u#price)
          )
          b#product
        in List.fold_left min (List.hd prices) (List.tl prices)
           <= cdata#actual_player#get_value_resource
      )
    in
    let find_u lu fail () =
      try last_selected <- Some ( `Unit (List.find oku lu) )
      with Not_found -> fail ()
    in
    let find_b lb fail () =
      try last_selected <- Some ( `Building (List.find okb lb) )
      with Not_found -> fail ()
    in
    let fail () =
      last_selected <- None
    in
    begin
      match last_selected with
      | None -> find_u lu (find_b lb fail)
      | Some (`Unit u) -> find_u (list_after u lu) (find_b lb (find_u lu fail))
      | Some (`Building b) ->
          find_b (list_after b lb) (find_u lu (find_b lb fail))
    end () ;
    begin match last_selected with
    | None -> ()
    | Some (`Unit u) -> cdata#camera#set_position u#position
    | Some (`Building b) -> cdata#camera#set_position b#position
    end

  (* Select next playable unit or building *)
  method private select_next =
    (* List of units *)
    let lu = cdata#actual_player#get_army
    (* List of buildings *)
    and lb = cdata#actual_player#get_buildings in
    self#select_playable lu lb

  (* Select previous playable unit or building *)
  method private select_pred =
    (* List of units *)
    let lu = List.rev cdata#actual_player#get_army
    (* List of buildings *)
    and lb = List.rev cdata#actual_player#get_buildings in
    self#select_playable lu lb

  val mutable music_run = ref true

  method paused =
    music_run := false

  method resumed =
    music_run := true

  initializer
    let actual_player = my_player#copy in
    cdata#init_core
      m_map
      actual_player
      (List.map
        (fun p ->
          if p#get_id = actual_player#get_id then
            (actual_player :> logicPlayer)
          else p#copy)
        m_players) ;
    (** Run music *)
    (* TODO *)
(*     Mood.init cdata; *)
(*     let music_player = MusicPlayer.music_player () in *)
(*     ignore @@ Thread.create (music_player#play_game) (music_run); *)

    cdata#init_buildings m_engine#get_neutral_buildings;
    cdata#init_interface m_camera

  (* Creates a menu for moving and attacking units *)
  (* Only creates the attack button if required *)
  method private create_disp_menu position atk =

    let disp_menu = new ingame_menu ~m_position:position ~m_width:150
      ~m_item_height:30 ~m_theme:Theme.yellow_theme
      ~m_bar_height:30 ~m_bar_icon:"menu_icon" ~m_bar_text:"Action" ()
    in

    let cursor = camera#cursor in

    if atk then begin
      new item "attack" "Attack" (fun () ->
        ui_manager#rem_widget disp_menu;
        ui_manager#unfocus disp_menu;
        match cursor#get_state with
        |Cursor.Displace(_,u,(r,_)) ->
          let in_range = Logics.units_inrange cursor#position
                (u#min_attack_range, u#attack_range)
                (cdata#actual_player :> Player.logicPlayer)
                cdata#players
          in
          let in_range =
            Fog.visible_army cdata#actual_player#get_fog in_range
          in
          if List.mem cursor#position r && in_range <> [] then begin
            cursor#set_state (Cursor.Action
              (u, cursor#position, in_range));
            camera#set_position (List.hd in_range)#position
          end else if List.mem cursor#position r then begin
            set_client_state (ClientPlayer.Received
              (cdata#current_move, Action.Wait));
            cursor#set_state Cursor.Idle
          end else
            cursor#set_state Cursor.Idle
        | _ -> assert false)
      |> disp_menu#add_child;
    end;

    new item "move" "Move" (fun () ->
      ui_manager#rem_widget disp_menu;
      ui_manager#unfocus disp_menu;
      set_client_state (ClientPlayer.Received
        (cdata#current_move, Action.Wait));
      cursor#set_state Cursor.Idle)
    |> disp_menu#add_child;

    let move_cancel () =
      ui_manager#rem_widget disp_menu;
      ui_manager#unfocus disp_menu;
      cursor#set_state Cursor.Idle
    in

    new item "cancel" "Cancel" move_cancel
    |> disp_menu#add_child;

    ui_manager#add_widget disp_menu;
    disp_menu#set_escape move_cancel;
    ui_manager#focus disp_menu


  method private create_ui =
    (* Main ingame menu *)
    let wsize = Window.size manager#window in
    let (w,h) = Vector2i.(wsize.x, wsize.y) in
    let my_menu = new ingame_menu
      ~m_position:(w / 2 - 75, 30) ~m_width:150
      ~m_item_height:30 ~m_theme:Theme.blue_theme
      ~m_bar_height:30 ~m_bar_icon:"menu_icon" ~m_bar_text:"Menu" () in

    (* Forfeit confirmation popup *)
    let forfeit_popup = new Windows.ingame_popup
      ~m_position:(w / 2 - 200,
        h / 2 - 80)
      ~m_size:(400, 110) ~m_theme:Theme.blue_theme
      ~m_text:("Do you really want to forfeit ? The game will be considered "
                ^ "lost...")
      ~m_bar_height:30 ~m_bar_icon:"menu_icon" ~m_bar_text:"Forfeit" in

    (* Buttons for the forfeit popup *)
    new Windows.text_framed_item
      (50, 70) (100, 25) "Yes !" (fun () -> m_engine#kill; Manager.manager#pop)
      Theme.blue_theme
    |> forfeit_popup#add_child;

    new Windows.text_framed_item
      (250, 70) (100, 25) "No !" (fun () -> ui_manager#unfocus forfeit_popup;
        forfeit_popup#toggle) Theme.blue_theme
    |> forfeit_popup#add_child;

    (* Button to open ingame menu *)
    let main_button = new key_button ~icon:"return"
      ~text:"Menu" ~m_size:(150, 30) ~keycode:(Keycode.Return)
      ~m_position:(w / 2 - 75, 0)
      ~callback:(fun () -> ())
      ~m_theme:Theme.blue_theme
    in

    main_button#set_callback (fun () ->
      if camera#cursor#get_state = Cursor.Idle then begin
        my_menu#toggle; main_button#toggle; ui_manager#focus my_menu
      end);

    (* Ingame menu items *)
    new item "cancel" "End turn" (fun () ->
      if uphandle#current_turn = Updates.Your_turn then
        set_client_state (ClientPlayer.Received ([], Action.End_turn));
      my_menu#toggle; main_button#toggle; ui_manager#unfocus my_menu)
    |> my_menu#add_child;

    new item "forfeit" "Forfeit" (fun () -> forfeit_popup#toggle;
      ui_manager#focus forfeit_popup; my_menu#toggle; main_button#toggle)
    |> my_menu#add_child;

    new item "params" "Settings" (fun () ->
      new SettingsScreen.state |> manager#push ;
      my_menu#toggle; main_button#toggle; ui_manager#unfocus my_menu)
    |> my_menu#add_child;

    let cancel_menu () =
      my_menu#toggle;
      main_button#toggle;
      ui_manager#unfocus my_menu
    in

    new item "cancel" "Cancel" cancel_menu
    |> my_menu#add_child;

    my_menu#set_escape cancel_menu;

    let cursor = cdata#camera#cursor in

    (* Attack menu items *)
    new item "fire" "Fire !" (fun () ->
      atk_menu#toggle;
      ui_manager#unfocus atk_menu;
      let cursor = cdata#camera#cursor in
      let atking_unit =
        match cursor#get_state with
        |Cursor.Action(u,_,_) -> u
        | _ -> assert false
      in
      let atked_unit  =
        match cdata#unit_at_position cursor#position with
        |Some(u) -> u
        |None -> assert false
      in
      set_client_state (ClientPlayer.Received
        (cdata#current_move,
         Action.Attack_unit (atking_unit#get_id, atked_unit#get_id)));
      cursor#set_state Cursor.Idle)
    |> atk_menu#add_child;

    let atk_cancel () =
      atk_menu#toggle;
      ui_manager#unfocus atk_menu;
      cursor#set_state Cursor.Idle
    in

    new item "cancel" "Cancel" atk_cancel
    |> atk_menu#add_child;

    atk_menu#set_escape atk_cancel;

    (* Build menu items *)

    let build_cancel () =
      build_menu#toggle;
      ui_manager#unfocus build_menu;
      cursor#set_state Cursor.Idle
    in

    new item "cancel" "Cancel" build_cancel
    |> build_menu#add_child;

    build_menu#set_escape build_cancel;

    my_menu#toggle;
    atk_menu#toggle;
    build_menu#toggle;

    forfeit_popup#toggle;

    ui_manager#add_widget forfeit_popup;
    ui_manager#add_widget main_button;
    ui_manager#add_widget my_menu;
    ui_manager#add_widget atk_menu;
    ui_manager#add_widget build_menu


  initializer
    self#create_ui;
    (*Thread.create (fun () -> m_engine#run) ()
    |> ignore*)

  val mutable last_event = 0.
  val mutable dir_key_pressed = false

  method private keyboard_events =
    let act_time = Unix.gettimeofday () in
    if (not ui_manager#is_focusing) &&
     act_time -. last_event >= 0.05 then Window.(
      last_event <- act_time;
      if Keyboard.is_pressed Keycode.Right ||
         Keyboard.is_pressed Keycode.Left  ||
         Keyboard.is_pressed Keycode.Up    ||
         Keyboard.is_pressed Keycode.Down  then
          dir_key_pressed <- true
      else
          dir_key_pressed <- false;
      if Keyboard.is_pressed Keycode.Right then
        camera#move (1,0);
      if Keyboard.is_pressed Keycode.Left then
        camera#move (-1,0);
      if Keyboard.is_pressed Keycode.Up then
        camera#move (0,-1);
      if Keyboard.is_pressed Keycode.Down then
        camera#move (0,1);
      if Keyboard.is_pressed Keycode.Z then
        camera#set_zoom (camera#zoom *. 1.1);
      if Keyboard.is_pressed Keycode.A then
        camera#set_zoom (camera#zoom *. 0.90909)
    )

  method handle_event e =
    if not (ui_manager#on_event e) then Event.(
      begin match e with
        | KeyPressed { KeyEvent.key = Keycode.Left; _ } ->
            if not dir_key_pressed then begin
              camera#move (-1,0);
              last_event <- Unix.gettimeofday() +. 0.2
            end

        | KeyPressed { KeyEvent.key = Keycode.Up; _ } ->
            if not dir_key_pressed then begin
              camera#move (0,-1);
              last_event <- Unix.gettimeofday() +. 0.2
            end

        | KeyPressed { KeyEvent.key = Keycode.Right; _ } ->
            if not dir_key_pressed then begin
              camera#move (1,0);
              last_event <- Unix.gettimeofday() +. 0.2
            end

        | KeyPressed { KeyEvent.key = Keycode.Down; _ } ->
            if not dir_key_pressed then begin
              camera#move (0,1);
              last_event <- Unix.gettimeofday() +. 0.2
            end

        | KeyPressed { KeyEvent.key = Keycode.Num0 ; _ } ->
            camera#set_zoom 1.

        | KeyPressed { KeyEvent.key = Keycode.M ; _ } ->
            camera#toggle_zoom

        | KeyPressed { KeyEvent.key = Keycode.S ; _ } ->
            uphandle#slower

        | KeyPressed { KeyEvent.key = Keycode.D ; _ } ->
            uphandle#faster

        | KeyPressed { KeyEvent.key = Keycode.X ; _ } ->
            self#select_next

        | KeyPressed { KeyEvent.key = Keycode.W ; _ } ->
            self#select_pred

        (*| KeyPressed { KeyEvent.key = Keycode.P ; _ } ->
            manager#window#capture 
            |> fun i -> i#save_to_file (Printf.sprintf "screenshot_%f.png"
              (Unix.gettimeofday ()))
            |> ignore*)

        | KeyPressed { KeyEvent.key = Keycode.Space ; _ } when
            uphandle#current_turn = Updates.Your_turn -> Cursor.(
              let cursor = cdata#camera#cursor in
              match cursor#get_state with
              |Idle -> begin
                match cdata#player_unit_at_position
                        cursor#position
                        cdata#actual_player with
                | Some u when (not u#has_played) ->
                    cursor#set_state (Displace (cdata#map, u,
                      Logics.accessible_positions u
                     (cdata#actual_player :> logicPlayer)
                      cdata#players
                      cdata#map))
                | None ->
                    (* We only check out buildings where there are no unit *)
                    begin match cdata#building_at_position cursor#position with
                      | (Some b, Some p) when
                        p#get_id = (cdata#actual_player :> logicPlayer)#get_id ->
                          if b#product <> [] then begin
                            (* Compute the list of producibles into a menu *)
                            build_menu#clear_children;
                            let item = new item "cancel" "Cancel" (fun () ->
                              build_menu#toggle;
                              ui_manager#unfocus build_menu;
                              cursor#set_state Cursor.Idle)
                            in item#toggle ; build_menu#add_child item;
                            List.iter (fun s ->
                              let u = List.find
                                (fun u -> u#name = s)
                                Config.config#unbound_units_list
                              in
                              new item
                                ~enabled:(u#price <= p#get_value_resource)
                                (Characters.handler#texture_from_id (p#get_id) s)
                                (s ^ " (" ^ (string_of_int u#price) ^ ")")
                                (fun () ->
                                  build_menu#toggle;
                                  ui_manager#unfocus build_menu;
                                  set_client_state (
                                    ClientPlayer.Received ([],
                                      Action.Create_unit (b#get_id,u)
                                    )
                                  ) ;
                                  cursor#set_state Cursor.Idle
                                )
                              |> (fun i -> i#toggle ; build_menu#add_child i)
                            ) b#product;
                            build_menu#toggle;
                            build_menu#set_position
                              (cdata#camera#project cursor#position) ;
                            ui_manager#focus build_menu ;
                            cursor#set_state (Build b)
                          end
                      | _ -> ()
                    end
                | _ -> ()
              end
              | Displace (_,u,(acc,_)) ->
                  let uopt = cdata#unit_at_position cursor#position in
                  let uopt = begin match uopt with
                    | Some u ->
                        let fog = cdata#actual_player#get_fog in
                        if Fog.hidden_unit fog u then None
                        else uopt
                    | None -> None
                  end in
                  begin match uopt with
                  | None when List.mem cursor#position acc ->
                     self#create_disp_menu
                        (cdata#camera#project cursor#position)
                        (Logics.units_inrange cursor#position
                          (u#min_attack_range, u#attack_range)
                          (cdata#actual_player :> Player.logicPlayer)
                          cdata#players <> [])
                  | Some u'
                    when u#get_id = u'#get_id && List.mem cursor#position acc ->
                      self#create_disp_menu
                        (cdata#camera#project cursor#position)
                        (Logics.units_inrange cursor#position
                          (u#min_attack_range, u#attack_range)
                          (cdata#actual_player :> Player.logicPlayer)
                          cdata#players <> [])
                  |_ ->
                      cursor#set_state Idle
                  end
              | Action(_,_,_) ->
                  atk_menu#toggle;
                  atk_menu#set_position (cdata#camera#project cursor#position);
                  ui_manager#focus atk_menu
              | _ -> ())

        | KeyPressed { KeyEvent.key = Keycode.Escape ; _ } ->
            cdata#camera#cursor#set_state Cursor.Idle
        | _ -> ()
      end)

  method render window =

    m_engine#run;
    self#keyboard_events;
    Interpolators.update () ;
    Window.clear window;

    cdata#minimap#compute cdata#map cdata#players;

    (* Rendering goes here *)
    Render.renderer#render_game window cdata uphandle;
    Render.renderer#draw_gui window ui_manager;

    Window.display window

  method destroy =
    music_run := false

end
