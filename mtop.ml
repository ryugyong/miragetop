module Main (C: Mirage_console_lwt.S) (S: Mirage_stack_lwt.V4)
         (Pclock: Mirage_types.PCLOCK) (DATA: Mirage_types_lwt.KV_RO)
         (KEYS: Mirage_types_lwt.KV_RO) (Http: Mtop_http.HTTP) = struct

  module Tcptop = Mtop_term.Tcptop(S)
  module Httop = Mtop_http.Httop(KEYS)(Pclock)(DATA)(Http)

  let start _console s _clock data keys http =
    Lwt.join [Httop.start data keys http; Tcptop.start s]
end
                                                                   
