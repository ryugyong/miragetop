

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


  let default () =
    let waiter, wakener = Lwt.wait () in
    let quit = [LTerm_edit.Custom (Lwt.wakeup wakener)] in
    let vbox = new LTerm_widget.vbox in
    let outwin = editor () in
    vbox#add outwin.hbox;
    let inpwin = editor () in
    inpwin.hbox#set_allocation
      { inpwin.hbox#allocation with row1 = inpwin.hbox#allocation.row1 - 10 };
    vbox#add ~expand:false inpwin.hbox;

    let send_key key =
      LTerm_edit.Custom (fun () -> vbox#send_event @@ LTerm_event.Key (make_key key)) in

    let tab =
      [{LTerm_key.control = false; meta = false; shift = false; code = LTerm_key.Tab}] in
    let send_key key =
      LTerm_edit.Custom (fun () -> vbox#send_event @@ LTerm_event.Key (make_key key)) in
    inpwin.edit#bind tab [send_key @@ `Other LTerm_key.Up];
    outwin.edit#bind tab [send_key @@ `Other LTerm_key.Down];

    LTerm.create (Lwt_unix.of_unix_file_descr (Unix.reader poke))
    
    
    
    


end

  
module Mtop = struct

  let map input inbuf outbuf =
    ();
end
