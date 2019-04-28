open Lwt.Infix

module type HTTP = Cohttp_lwt.S.Server

(* Logging *)
let https_src = Logs.Src.create "https" ~doc:"HTTPS server"
module Https_log = (val Logs.src_log https_src : Logs.LOG)

let http_src = Logs.Src.create "http" ~doc:"HTTP server"
module Http_log = (val Logs.src_log http_src : Logs.LOG)

module Httop (KEYS: Mirage_types_lwt.KV_RO) (Pclock: Mirage_types.PCLOCK) (FS: Mirage_types_lwt.KV_RO) (S: HTTP) = struct
  module X509 = Tls_mirage.X509(KEYS)(Pclock)

  let failf fmt = Fmt.kstrf Lwt.fail_with fmt

  (* given a URI, find the appropriate file,
   * and construct a response with its contents. *)
  let rec dispatcher fs uri =
    match Uri.path uri with
    | "" | "/" -> dispatcher fs (Uri.with_path uri "index.html")
    | path ->
       let header =
         Cohttp.Header.init_with "Strict-Transport-Security" "max-age=31536000"
       in
       let mimetype = Magic_mime.lookup path in
       let headers = Cohttp.Header.add header "content-type" mimetype in
       Lwt.catch
         (fun () ->
           FS.get fs (Mirage_kv.Key.v path) >>= function
           | Error e -> failf "get: %a" FS.pp_error e
           | Ok body ->
              S.respond_string ~status:`OK ~body ~headers ())
         (fun _exn ->
           S.respond_not_found())

  (* Redirect to the same address, but in https. *)
  let redirect port uri =
    let new_uri = Uri.with_scheme uri (Some "https") in
    let new_uri = Uri.with_port new_uri (Some port) in
    Http_log.info (fun f -> f "[%s] -> [%s]"
                              (Uri.to_string uri) (Uri.to_string new_uri)
      );
    let headers = Cohttp.Header.init_with "location" (Uri.to_string new_uri) in
    S.respond ~headers ~status:`Moved_permanently ~body:`Empty ()

  let serve dispatch =
    let callback (_, cid) request _body =
      let uri = Cohttp.Request.uri request in
      let cid = Cohttp.Connection.to_string cid in
      Https_log.info (fun f-> f "[%s] serving %s." cid (Uri.to_string uri));
      dispatch uri
    in
    let conn_closed (_,cid) =
      let cid = Cohttp.Connection.to_string cid in
      Https_log.info (fun f-> f "[%s] closing" cid);
    in
    S.make ~conn_closed ~callback ()

  let tls_init kv =
    X509.certificate kv `Default >>= fun cert ->
    let conf = Tls.Config.server ~certificates:(`Single cert) () in
    Lwt.return conf

  let start data keys http =
    tls_init keys >>= fun cfg ->
    let https_port = Key_gen.https_port () in
    let tls = `TLS (cfg, `TCP https_port) in
    let http_port = Key_gen.http_port () in
    let tcp = `TCP http_port in
    let https =
      Https_log.info (fun f -> f "listening on %d/TCP" https_port);
      http tls @@ serve (dispatcher data)
    in
    let http =
      Http_log.info (fun f -> f "listening of %d/TCP" http_port);
      http tcp @@ serve (redirect https_port)
    in
    Lwt.join [ https; http ]
end
                
module Tcptop (S: Mirage_stack_lwt.V4) = struct
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
  let start s =
    let port = Key_gen.telnet_port () in
    S.listen_tcpv4 s ~port (fun flow ->
        let stdout = Format.make_formatter (tcp_write flow) (fun a -> a) in
        Clflags.native_code := true;
        Compmisc.init_path true;
        let dst, dst_port = S.TCPV4.dst flow in
        Logs.info (fun f -> f "new tcp connection from IP %s on port %d"
                              (Ipaddr.V4.to_string dst) dst_port);
        Toploop.read_interactive_input :=
          (fun prompt buffer len ->
            tcp_write flow prompt 0 (String.length prompt);
            match tcp_read flow with
            | None -> ignore (S.TCPV4.close flow);
                      (0, true)
            | Some b -> Bytes.blit b 0 buffer 0 (min (Bytes.length b) len);
                        (Bytes.length b, false););
        Toploop.loop stdout;
        
        S.TCPV4.close flow;
      );
    S.listen s
end
    
module Main (C: Mirage_console_lwt.S) (S: Mirage_stack_lwt.V4)
         (Pclock: Mirage_types.PCLOCK) (DATA: Mirage_types_lwt.KV_RO)
         (KEYS: Mirage_types_lwt.KV_RO) (Http: HTTP) = struct

  module Tcptop = Tcptop(S)
  module Httop = Httop(KEYS)(Pclock)(DATA)(Http)

  let start _console s _clock data keys http =

    Lwt.join [Httop.start data keys http; Tcptop.start s]
end
                                                                   
