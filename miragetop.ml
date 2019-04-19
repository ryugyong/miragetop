open Clflags
open Lwt.Infix
module Main (C: Mirage_console_lwt.S) (S: Mirage_stack_lwt.V4) = struct
  let preload_objects = ref []

  let tcp_write flow str =
    ignore (S.TCPV4.write flow (Cstruct.of_string str) >>= function
            | Error e ->
               Logs.warn
                 (fun f -> f "Error writing data to established connection %a"
                             S.TCPV4.pp_write_error e); Lwt.return_unit;
            | Ok () -> Lwt.return_unit); ()

  let tcp_read flow =
    Lwt_main.run (
        S.TCPV4.read flow >>= function
        | Ok `Eof -> Logs.info (fun f -> f "Closing connection!");
                     Lwt.return_none
        | Error e ->
           Logs.warn
             (fun f -> f "Error reading data from established connection %a"
                         S.TCPV4.pp_error e); Lwt.return_none
        | Ok (`Data b) ->
           Logs.debug (fun f -> f "read: %d bytes:\n%s" (Cstruct.len b)
                                  (Cstruct.to_string b));
           Lwt.return_some (Cstruct.to_bytes b))
    
  let prepare ppf =
    Opttoploop.set_paths ();
    try
      let res =
        List.for_all (Opttopdirs.load_file ppf) (List.rev !preload_objects)
      in !Opttoploop.toplevel_startup_hook (); res
    with x ->
      try Location.report_exception ppf x; false
      with x ->
        Format.fprintf ppf "Uncaught exception: %s\n" (Printexc.to_string x);
        false

  let start _ s =
    native_code := true;
    if not (prepare Format.err_formatter) then exit 2;
    Compmisc.init_path true;
    let port = Key_gen.port () in
    S.listen_tcpv4 s ~port (fun flow ->
        let dst, dst_port = S.TCPV4.dst flow in
        Logs.info (fun f -> f "new tcp connection from IP %s on port %d"
                              (Ipaddr.V4.to_string dst) dst_port);
        Opttoploop.read_interactive_input :=
          (fun prompt buffer len ->
          tcp_write flow prompt;
          match tcp_read flow with
          | None -> ignore (S.TCPV4.close flow);
                    (0, true)
          | Some b -> Logs.debug (fun f -> f "read: %d bytes:\n%s"
                                             (Bytes.length b)
                                             (Bytes.to_string b));
                      (* Bytes.blit b 0 buffer 0 len; *)
                      (Bytes.length b, false););
        Opttoploop.loop (Format.make_formatter
                           (fun str _ _ ->
                             tcp_write flow str)
                           (fun a -> a));
        Lwt.return_unit
      );
    S.listen s
end
                                                                   
