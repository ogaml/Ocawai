open OcsfmlGraphics
open Utils

open Manager

open Player
open Menus

class game = object(self)

  inherit State.state as super

  val generator = new FieldGenerator.t 100 100 1

  val ui_manager = new UIManager.ui_manager

  val camera = new Camera.camera
    ~def_tile_size:50
    ~w:manager#window#get_width ~h:manager#window#get_height
    ~maxpos:(Position.create (99,99))

  val num = 0

  (* val cdata = new ClientData.client_data ~camera
    ~map:(generator#field)
    ~units:(List.nth (generator#armies) num) *)

  val mutable cdata : ClientData.client_data option = None

  method private create_ui =
    let my_menu = new menu (manager#window#get_width / 2 - 50, 0) 150 30
    OcsfmlWindow.KeyCode.Return in

    new item "forfeit" "Forfeit" (fun () -> print_endline "forfeited" ; Manager.manager#pop)
    |> my_menu#add_child;

    new item "info" "Info" (fun () -> print_endline "info activated";
      my_menu#toggle)
    |> my_menu#add_child;

    new item "params" "Settings" (fun () -> print_endline "settings activated"; 
      my_menu#toggle)
    |> my_menu#add_child;

    new item "infantry" "Cancel" (fun () -> print_endline "canceled"; 
      my_menu#toggle)
    |> my_menu#add_child;

    let main_button = new key_button ~icon:"return"
      ~text:"Menu" ~m_size:(150, 30) ~keycode:(OcsfmlWindow.KeyCode.Return)
      ~m_position:(manager#window#get_width / 2 - 50, 0)
      ~callback:(fun () -> my_menu#toggle)
    in

    (* Add the button before the menu, so that the menu will display on top of
     * the button (yes, this is bad, we need to implement focusing) *)
    ui_manager#add_widget (main_button :> Widget.widget);
    main_button#toggle;
    ui_manager#add_widget (my_menu :> Widget.widget)

  initializer
    self#create_ui;
    cdata <-Some (new ClientData.client_data ~camera
      ~map:(generator#field)
      ~units:(List.nth (generator#armies) num))

  method handle_event e =
    (* Ugly *)
    let cdata = match cdata with
      | Some c -> c
      | None -> failwith "Oh no !\n"
    in

    if not (ui_manager#on_event e) then OcsfmlWindow.Event.(
      begin match e with
        | KeyPressed { code = OcsfmlWindow.KeyCode.Right ; _ } ->
            camera#move (1,0)

        | KeyPressed { code = OcsfmlWindow.KeyCode.Down ; _ } ->
            camera#move (0,1)

        | KeyPressed { code = OcsfmlWindow.KeyCode.Left ; _ } ->
            camera#move (-1,0)

        | KeyPressed { code = OcsfmlWindow.KeyCode.Up ; _ } ->
            camera#move (0,-1)

        | KeyPressed { code = OcsfmlWindow.KeyCode.T ; _ } ->
            camera#set_position (Position.create (80,80))

        | KeyPressed { code = OcsfmlWindow.KeyCode.Z ; _ } ->
            camera#set_zoom (camera#zoom *. 1.1)

        | KeyPressed { code = OcsfmlWindow.KeyCode.A ; _ } ->
            camera#set_zoom (camera#zoom *. 0.90909)

        | KeyPressed { code = OcsfmlWindow.KeyCode.Num0 ; _ } ->
            camera#set_zoom 1.

        | KeyPressed { code = OcsfmlWindow.KeyCode.Space ; _ } ->
            begin
              match cdata#selected with
              | Some u ->
                  cdata#unselect;
                  cdata#camera#cursor#stop_moving
              | None ->
                  begin match cdata#unit_at_position
                    cdata#camera#cursor#position with
                    | Some u ->
                        cdata#select_unit u;
                        cdata#camera#cursor#set_moving
                    | None -> ()
                  end
            end

          | _ -> ()
      end)

  method render window =

    super#render window ;
    Interpolators.update () ;
    window#clear ();

    (* Ugly *)
    let cdata = match cdata with
      | Some c -> c
      | None -> failwith "Oh no !\n"
    in

    (* Rendering goes here *)
    Render.render_game window cdata;
    Render.draw_hud window;
    Render.draw_gui window ui_manager;

    window#display

end
