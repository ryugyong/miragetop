open Lwt.Infix
open Cow.Html

(* Logging *)
let mtop_html = Logs.Src.create "mtop_html" ~doc:"Hypertop Module"
module Log = (val Logs.src_log mtop_html : Logs.LOG)


module Hypertop (S: Cohttp_lwt.S.Server) (DATA: Mirage_types_lwt.KV_RW) = struct
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
                        width: 500px;
                        display: block;
                        }
                        .raise, input {
                        border-radius: 5px;
                        border: 1px solid #ccc;
                        box-shadow: 1px 1px 1px #999;
                        margin: 5px;
                        }
                        input, .status {
                        padding: 2px 8px;
                        }
                        input[type="submit"]:hover {
                        color: #333333;
                        background-color: #DDDDDD;
                        }
                        input[type="submit"]:active {
                        color: #DDDDDD;
                        background-color: #000000;
                        box-shadow: 0;
                        }
                        |}]))
      ++ body the_body in
    let b = Buffer.create 16 in
    Cow.Html.output_doc ~nl:true (`Buffer b) dom;
    Buffer.contents b
    
  let repl_page ?(msg="") ?(code="new file") ?(res="") key =
    let code =
      (tag ~attrs:[("action","/top/"^key);("method","post")] "form"
         (list
            [tag ~cls:"raise"
               ~attrs:[("name", "sequence");
                       ("autocapitalize", "none"); (* iOS *)
                       ("autocomplete", "off"); ("autofocus", "true");
                       ("spellcheck", "false"); ("wrap", "soft");
                       ("rows", "10"); ("cols", "50")] "textarea" (string code);
             input ~ty:"submit" ~cls:"raise" ~attrs:[("name","eval")] "eval";
             input ~ty:"submit" ~cls:"raise" ~attrs:[("name","save")] "save";
             input ~cls:"raise" ~ty:"text" ~attrs:[("placeholder","key")] key;
             span ~cls:"status" (string msg)])) in
    let body = div ~cls:"code" code
               ++ div ~cls:"interface raise" (string res) in
    page ~title:"mtop" body

  let split_path path =
    let rec aux = function
      | [] | [""] -> []
      | hd::tl -> hd :: aux tl
    in
    List.filter (fun e -> e <> "") (aux (Re.Str.(split_delim (regexp_string "/") path)))
    
  let dispatcher fs uri meth body =
    let path = split_path @@ Uri.path uri in
    Log.info (fun f -> f "dispatching %s" @@ String.concat "/" path);
    match path with
    | [] | [""] | ["";""] ->
       let uri = Uri.of_string in
       let body = a ~href:(uri "/top") (img (uri "/NJT1369.gif")) in
       let headers = Cohttp.Header.add headers "content-type" "text/html" in
       S.respond_string ~status:`OK ~body:(page ~title:"hey" body) ~headers ()
    | "top" :: key ->
       let path = String.concat "/" key in
       (match meth with
         | `POST ->
          Log.info (fun f -> f "/repl POST");
          body >>= fun body ->
            let code = query_val body "sequence" in
            Log.info (fun f -> (f "eval:\n %s" code));
            DATA.set fs (Mirage_kv.Key.v path) code >>= fun r ->
            let status,msg = match r with
              | Error _e -> (`Internal_server_error,"failed to save")
              | Ok _ -> (`OK,"saved") in
            S.respond_string ~status ~headers
              ~body:(repl_page ~msg ~code ~res:(Shell.eval code) path) ()
       | _ ->
          Log.info (fun f -> f "fetching %s" path);
          DATA.get fs (Mirage_kv.Key.v path) >>= function
          | Error _e ->
             S.respond_string ~headers ~status:`Not_found
               ~body:(repl_page ~code:"(* new file *)" path) ()
          | Ok sequence -> 
             S.respond_string ~headers ~status:`OK ~body:(repl_page ~code:sequence path) ())
    | _ ->
       raise @@ Failure ("not handled"^Uri.path uri)
       

end
                                             
