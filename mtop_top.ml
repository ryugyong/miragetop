


module Top = struct

  let eval s =
    let b = Buffer.create 0 in
    Toploop.eval s b;
    Buffer.contents b;


end
