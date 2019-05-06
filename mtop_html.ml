open Lwt.Infix
open Cow.Html

(* Logging *)
let mtop_html = Logs.Src.create "mtop_html" ~doc:"Hypertop Module"
module Log = (val Logs.src_log mtop_html : Logs.LOG)


module Hypertop (S: Cohttp_lwt.S.Server) (DATA: Mirage_types_lwt.KV_RO) = struct
  module Shell = Mtop_shell.Shell

  let headers =
    Cohttp.Header.init_with "Strict-Transport-Security" "max-age=31536000"

  let query_val q k =
    String.concat "" @@
      match Uri.get_query_param' (Uri.of_string ("/?"^q)) k with
      | None -> ["None???"] | Some e -> e
    
  let page ?(title="") the_body =
    let dom =
      (head (list [
                 Cow.Html.title (string title);
                 style {|
                        body {
                        font-size: .8rem;
                        letter-spacing: 1px;
                        }
                        textarea, .interface {
                        white-space: pre-wrap;
                        padding: 10px;
                        line-height: 1.5;
                        border-radius: 5px;
                        border: 1px solid #ccc;
                        box-shadow: 1px 1px 1px #999;
                        width: 500px;
                        }
                        label, textarea, button {
                        display: block;
                        margin-bottom: 10px;
                        }
                        |}]))
      ++ body the_body in
    let b = Buffer.create 16 in
    Cow.Html.output_doc ~nl:true (`Buffer b) dom;
    Buffer.contents b
    
  let repl_page ?(code="new file") ?(res="") () =
    let body =
    (div ~cls:"code" (tag ~attrs:[("action","/repl");("method","post")] "form"
                        (list [
                             tag ~attrs:[("name", "sequence");
                                          ("id", "sequence");
                                          ("autocapitalize", "none"); (* iOS *)
                                          ("autocomplete", "off");
                                          ("autofocus", "true");
                                          ("spellcheck", "false");
                                          ("wrap", "soft");
                                          ("rows", "10");
                                          ("cols", "50")] "textarea" (string code);
                             br;
                             tag ~attrs:[("type","submit")] "button" (string "eval")])))
    ++ (div ~cls:"interface" (tag "samp" (string res))) in
    page ~title:"mtop" body

  let dispatcher fs uri meth body =
    let path = Uri.path uri in
    match path with
    | "" | "/" ->
       let uri = Uri.of_string in
       let body = a ~href:(uri "/repl") (img (uri "/NJT1369.gif")) in
       let headers = Cohttp.Header.add headers "content-type" "text/html" in
       S.respond_string ~status:`OK ~body:(page ~title:"hey" body) ~headers ()
    | "/repl" ->
      ( match meth with
       | `POST ->
          Log.info (fun f -> f "/repl POST");
          body >>= fun body ->
            let code = query_val body "sequence" in
            Log.info (fun f -> (f "eval:\n %s" code));
            S.respond_string ~status:`OK
              ~body:(repl_page ~code ~res:(Shell.eval code) ()) ~headers ()
       | _ ->
          DATA.get fs (Mirage_kv.Key.v "test") >>= function
          | Error _e ->
             S.respond_string ~headers ~status:`Not_found ~body:(repl_page ()) ()
          | Ok sequence -> 
             S.respond_string ~headers ~status:`OK ~body:(repl_page ~code:sequence ()) ())
    | _path ->
       raise @@ Failure ("not handled"^path)
       

end
                                             
