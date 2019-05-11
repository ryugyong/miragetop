open Lwt.Infix
include Key_gen
      
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
        let dst, dst_port = S.TCPV4.dst flow in
        Logs.info (fun f -> f "new tcp connection from IP %s on port %d"
                              (Ipaddr.V4.to_string dst) dst_port);
        Loop.read_interactive_input :=
          (fun prompt buffer len ->
            tcp_write flow prompt 0 (String.length prompt);
            match tcp_read flow with
            | None -> ignore (S.TCPV4.close flow);
                      (0, true)
            | Some b -> Bytes.blit b 0 buffer 0 (min (Bytes.length b) len);
                        (Bytes.length b, false););
        Loop.loop stdout;
        
        S.TCPV4.close flow;
      );
    S.listen s
end

module Termif = struct

  type editor = {
      edit : LTerm_edit.edit;
      hbox : LTerm_widget.hbox;
      vscroll : LTerm_widget.vscrollbar;
    }
      
  let editor () =
    let hbox = new LTerm_widget.hbox in
    let edit = new LTerm_edit.edit () in
    let vscroll = new LTerm_widget.vscrollbar ~width:1 edit#vscroll in
    hbox#add edit;
    hbox#add ~expand:false vscroll;
    {edit; hbox; vscroll}
    
  let make_key ?(ctrl = false) ?(meta = false) ?(shift = false) c =
    let code =
      match c with
      | `Char c -> LTerm_key.Char (CamomileLibrary.UChar.of_char c)
      | `Other key -> key in
    { LTerm_key.control = ctrl; meta; shift; code }


  (* let default () =
   *   let waiter, wakener = Lwt.wait () in
   *   let quit = [LTerm_edit.Custom (Lwt.wakeup wakener)] in
   *   let vbox = new LTerm_widget.vbox in
   *   let outwin = editor () in
   *   vbox#add outwin.hbox;
   *   let inpwin = editor () in
   *   inpwin.hbox#set_allocation
   *     { inpwin.hbox#allocation with row1 = inpwin.hbox#allocation.row1 - 10 };
   *   vbox#add ~expand:false inpwin.hbox;
   * 
   *   let send_key key =
   *     LTerm_edit.Custom (fun () -> vbox#send_event @@ LTerm_event.Key (make_key key)) in
   * 
   *   let tab =
   *     [{LTerm_key.control = false; meta = false; shift = false; code = LTerm_key.Tab}] in
   *   let send_key key =
   *     LTerm_edit.Custom (fun () -> vbox#send_event @@ LTerm_event.Key (make_key key)) in
   *   inpwin.edit#bind tab [send_key @@ `Other LTerm_key.Up];
   *   outwin.edit#bind tab [send_key @@ `Other LTerm_key.Down];
   * 
   *   (\* LTerm.create (Lwt_unix.of_unix_file_descr (Unix.reader poke)) *\) *)
    
    
    
    


end

  
