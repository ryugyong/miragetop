
open Clflags 
open Lwt.Infix
module Main (C: Mirage_console_lwt.S) (S: Mirage_stack_lwt.V4) = struct
  let tcp_write flow str off len =
    ignore (S.TCPV4.write flow (Cstruct.of_string ~off ~len str) >>= function
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
    
  let start _ s =
    let port = Key_gen.port () in
    S.listen_tcpv4 s ~port (fun flow ->
        let stdout = Format.make_formatter (tcp_write flow) (fun a -> a) in
        native_code := true;
        Compmisc.init_path true;
        let dst, dst_port = S.TCPV4.dst flow in
        Logs.info (fun f -> f "new tcp connection from IP %s on port %d"
                              (Ipaddr.V4.to_string dst) dst_port);
(*        Toploop.read_interactive_input :=
          (fun prompt buffer len ->
            tcp_write flow prompt 0 (String.length prompt);
            match tcp_read flow with
            | None -> ignore (S.TCPV4.close flow);
                      (0, true)
            | Some b -> Bytes.blit b 0 buffer 0 (min (Bytes.length b) len);
                        (Bytes.length b, false););*)
        Toploop.loop stdout;
        
        S.TCPV4.close flow;
      );
    S.listen s
end
                                                                   
