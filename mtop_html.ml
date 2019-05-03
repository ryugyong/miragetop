open Lwt.Infix
open Cow.Html

(* Logging *)
let mtop_html = Logs.Src.create "mtop_html" ~doc:"Hypertop Module"
module Log = (val Logs.src_log mtop_html : Logs.LOG)


module Hypertop (S: Cohttp_lwt.S.Server) = struct

  let headers =
    Cohttp.Header.init_with "Strict-Transport-Security" "max-age=31536000"

  let page ?(title="") the_body =
    let b = Buffer.create 16 in
    (Cow.Html.output_doc ~nl:true (`Buffer b)
       (head
          (Cow.Html.title (string title))
        ++ body the_body));
    Buffer.contents b
    
  let repl_page ?(res="") () =
    let body =
    (div ~cls:"code" (tag ~attrs:[("action","/repl");("method","post")] "form"
                        (list [
                             (tag "textarea" (string "the code" ));
                             (tag ~attrs:[("type","submit")] "button" (string "eval"))])))
    ++ (div (string res)) in
    page ~title:"mtop" body
          
  let dispatcher uri meth body =
    match Uri.path uri with
    | "" | "/" ->
       let uri = Uri.of_string in
       let body = a ~href:(uri "/repl") (img (uri "/NJT1369.gif")) in
       let headers = Cohttp.Header.add headers "content-type" "text/html" in
       S.respond_string ~status:`OK ~body:(page ~title:"hey" body) ~headers ()
    | "/repl" -> (match meth with
                  | `POST ->
                     Log.info (fun f -> f "/repl POST");
                     body >>= (fun body ->
                       Log.info (fun f -> f "eval:\n %s" body);
                       S.respond_string ~status:`OK ~body:(
                           repl_page ~res:(Mtop_top.Top.eval body) ()) ~headers ())
                  | _ ->
                     S.respond_string ~status:`OK ~body:(repl_page ()) ~headers ())
                   
    | _path ->
       raise (Failure "not handled")
       

end
                                             
