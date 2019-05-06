

module Shell = struct

  
  let init () =
    Clflags.native_code := true;
    Compmisc.init_path true
    
  let eval s =
    let b = Buffer.create 0 in
    try
      Toploop.eval s b;
      Buffer.contents b
    with e -> (Printexc.to_string e)^"\n"^(Printexc.get_backtrace ());
            
end
