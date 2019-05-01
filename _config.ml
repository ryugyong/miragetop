

open Mirage

let stack = generic_stackv4 default_network
let data_key = Key.(value @@ kv_ro ~group:"data" ())
let data = generic_kv_ro ~key:data_key "htdocs"
let https_srv = http_server @@ conduit_direct ~tls:true stack
let http_port =
  let doc = Key.Arg.info ~doc:"Listening HTTP port." ["http"] in
  Key.(create "http_port" Arg.(opt int 8080 doc))
let https_port =
  let doc = Key.Arg.info ~doc:"Listening HTTPS port." ["https"] in
  Key.(create "https_port" Arg.(opt int 4433 doc))
let certs_key = Key.(value @@ kv_ro ~group:"certs" ())
let certs = generic_kv_ro ~key:certs_key "tls"

let telnet_port =
  let doc = Key.Arg.info
              ~doc:"The TCP port on which to listen for incoming telnet-style connections."
              ["port"] in
  Key.(create "telnet_port" Arg.(opt int 10018 doc))
  
  
let main =
  
  let packages = [
      package "uri"; package "magic-mime";
      package "duration";
      package "lambda-term";
      package ~ocamlfind:["compiler-libs.common";
                          "compiler-libs.optcomp";
                          "compiler-libs.toplevel";
                          "compiler-libs"] "ocaml-compiler-libs";] in
  let keys = List.map Key.abstract [ http_port; https_port; telnet_port ] in
  foreign ~keys ~packages "Mtop.Main"
    (console @-> stackv4 @-> pclock @-> kv_ro @-> kv_ro @-> http @-> job)
  
          
let () =
  register "miragetop" [
      main $ default_console $ stack $ default_posix_clock $ data $ certs $ https_srv
    ]
