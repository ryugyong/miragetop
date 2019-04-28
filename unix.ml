


module Unix = struct
  type reader = function () -> Bytes.t
  type writer = (Bytes.t -> ())
  type file_descr = reader | writer;

  let reader (() -> string) = (* stdin *)
    file_descr

  let read fd buf ofs len =
    0

  let write fd buf ofs len =
    0
end
                
         
