(** Launcher of the interface *)

open Manager

let () = begin

  (new MainMenu.main_menu :> State.state) |> manager#push ;
  manager#run;
  Config.config#save_all
end
