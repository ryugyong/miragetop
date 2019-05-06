open Lwt.Infix


module type HTTP = Cohttp_lwt.S.Server
               

(* Logging *)
let https_src = Logs.Src.create "https" ~doc:"HTTPS server"
module Https_log = (val Logs.src_log https_src : Logs.LOG)


let http_src = Logs.Src.create "http" ~doc:"HTTP server"
module Http_log = (val Logs.src_log http_src : Logs.LOG)


module Httop (KEYS: Mirage_types_lwt.KV_RO) (Pclock: Mirage_types.PCLOCK)
         (DATA: Mirage_types_lwt.KV_RO) (S: HTTP) = struct

  module X509 = Tls_mirage.X509(KEYS)(Pclock)
  module Hypertop = Mtop_html.Hypertop(S)(DATA)

  let headers = Cohttp.Header.init_with "Strict-Transport-Security" "max-age=31536000"

  let dispatcher fs uri meth body =
    Lwt.catch
      (fun () ->
        Hypertop.dispatcher fs uri meth (Cohttp_lwt.Body.to_string body))
      (fun _exn -> 
        let path = Uri.path uri in
        let headers = Cohttp.Header.add headers "content-type" (Magic_mime.lookup path) in
           Lwt.catch
             (fun () ->
               DATA.get fs (Mirage_kv.Key.v path) >>= function
               | Error e -> (Fmt.kstrf Lwt.fail_with "get: %a") DATA.pp_error e
               | Ok body -> S.respond_string ~status:`OK ~body ~headers ())
             (fun _exn -> S.respond_not_found()))

  (* Redirect to the same address, but in https. *)
  let redirect port uri _meth _body =
    let new_uri = Uri.with_scheme uri (Some "https") in
    let new_uri = Uri.with_port new_uri (Some port) in
    Http_log.info (fun f -> f "[%s] -> [%s]"
                              (Uri.to_string uri) (Uri.to_string new_uri)
      );
    let headers = Cohttp.Header.init_with "location" (Uri.to_string new_uri) in
    S.respond ~headers ~status:`Moved_permanently ~body:`Empty ()

  let serve dispatch =
    let callback (_, cid) request body =
      let uri = Cohttp.Request.uri request in
      let meth = Cohttp.Request.meth request in
      Https_log.info (fun f-> f "[%s] serving %s." (Cohttp.Connection.to_string cid)
                                (Uri.to_string uri));
      dispatch uri meth body
    in
    let conn_closed (_,cid) = Https_log.info (fun f-> f "[%s] closing"
                                                        (Cohttp.Connection.to_string cid)); in
    S.make ~conn_closed ~callback ()

  let tls_init kv =
    X509.certificate kv `Default >>= fun cert ->
    let conf = Tls.Config.server ~certificates:(`Single cert) () in
    Lwt.return conf

  let start data keys http =
    tls_init keys >>= fun cfg ->
    let https_port = Key_gen.https_port () in
    let http_port = Key_gen.http_port () in
    let tls = `TLS (cfg, `TCP https_port) in
    let tcp = `TCP http_port in
    let https =
      Https_log.info (fun f -> f "listening on %d/TCP" https_port);
      http tls @@ serve (dispatcher data)
    in
    let http =
      Http_log.info (fun f -> f "listening on %d/TCP" http_port);
      http tcp @@ serve (redirect https_port)
    in
    Lwt.join [ https; http ]
end
