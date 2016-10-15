(** Launcher of the interface *)

open Manager

let () = begin

(*   TODO (new MainMenu.main_menu :> State.state) |> manager#push ; *)
  manager#run;
  Config.config#save_all
end
