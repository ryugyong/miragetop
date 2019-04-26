open Format

(* Set the load paths, before running anything *)
val set_paths : unit -> unit

(* The interactive toplevel loop *)
val loop : formatter -> unit
  
(* Typing environment for the toplevel *)
val toplevel_env : Env.t ref

(* Initialize the typing environment for the toplevel *)
val initialize_toplevel_env : unit -> unit



(* Hooks for external line editor *)
val read_interactive_input : (string -> bytes -> int -> int * bool) ref

(* Hooks for initialization *)
val toplevel_startup_hook : (unit -> unit) ref
