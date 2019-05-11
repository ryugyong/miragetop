
module Main (C: Mirage_console_lwt.S) (S: Mirage_stack_lwt.V4)
         (Pclock: Mirage_types.PCLOCK) (DATA: Mirage_types_lwt.KV_RW)
         (KEYS: Mirage_types_lwt.KV_RO) (Http: Mtop_http.HTTP) = struct

  module Tcptop = Mtop_term.Tcptop(S)
  module Httop = Mtop_http.Httop(KEYS)(Pclock)(DATA)(Http)
  module Shell = Shell.Shell(DATA)
  (* module Store = Irmin_mirage.Git.KV(Irmin_git.G)(Irmin.Contents.S) *)
               

  let start _console s _clock data keys http =
    print_string "this is start\n";
    Shell.init data;
    Lwt.join [Httop.start data keys http; Tcptop.start s]
end
                                                                   
