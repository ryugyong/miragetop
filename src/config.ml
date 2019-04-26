

open Mirage

let port =
  let doc = Key.Arg.info ~doc:"The TCP port on which to listen for incoming connections." ["port"] in
  Key.(create "port" Arg.(opt int 8080 doc))
  
let main =
  
  let packages = [
      package "duration";
      package ~ocamlfind:["compiler-libs.common";
                          "compiler-libs.optcomp";
                          (* "compiler-libs.toplevel"; *)
                          "compiler-libs"] "";] in
  foreign ~keys:[Key.abstract port] ~packages "Miragetop.Main" (console @-> stackv4 @-> job)
  
let stack = generic_stackv4 default_network
          
let () =
  register "miragetop" [
      main $ default_console $ stack
    ]
