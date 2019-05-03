open Lwt.Infix
open Cow.Html

(* Logging *)
let mtop_html = Logs.Src.create "mtop_html" ~doc:"Hypertop Module"
module Log = (val Logs.src_log mtop_html : Logs.LOG)


module Hypertop (S: Cohttp_lwt.S.Server) = struct

  let headers =
    Cohttp.Header.init_with "Strict-Transport-Security" "max-age=31536000"

  let query_val q k =
    String.concat "" @@
      match Uri.get_query_param' (Uri.of_string ("/?"^q) ) k with
      | None -> ["None???"] | Some e -> e
    
  let page ?(title="") the_body =
    let b = Buffer.create 16 in
    (Cow.Html.output_doc ~nl:true (`Buffer b)
       (head
          (Cow.Html.title (string title))
        ++ body the_body));
    Buffer.contents b
    
  let repl_page ?(code="") ?(res="") () =
    let body =
    (div ~cls:"code" (tag ~attrs:[("action","/repl");("method","post")] "form"
                        (list [
                             (tag ~attrs:[("name","code")] "textarea" (string code ));
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
                       let code = query_val body "code" in
                       Log.info (fun f -> (f "eval:\n %s" code));
                       S.respond_string ~status:`OK ~body:(
                           repl_page ~code ~res:(Mtop_top.Top.eval code) ()) ~headers ())
                  | _ ->
                     S.respond_string ~status:`OK ~body:(repl_page ()) ~headers ())
                   
    | _path ->
       raise (Failure "not handled")
       

end
                                             
