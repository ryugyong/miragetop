

(* So, the stores huh.
 * 
 * We want to be able to save chunks of code, basically modules.
 * 
 * ALL OTHER TYPES OF FILES WILL BE CRUNCHED
 * this allows types! and all sorts of other niceties
 * And then you get modules as your structure :)
 * 
 * Organization structure for humans:
 * - At least tree structure, graph structure prefered
 * - Will need "folder" names, come up with a better name than folder tho.
 * - What if we used git branches of just single level key/value stores.
 * - File names
 * - File types
 * - no permissions, no file extensions 
 * -  
 * 
 * Entry point and scope for machine initialization:
 * - If you make the whole key/value store in scope by default 
 * -- Will that be slow?
 * -- You could use Irmin branches to separate code you do not want to use. 
 * - how do we seperate interfaces from implementation? (mli/ml)
 * 


 * module A: sig (* contents of file A.mli *) end
 *         = struct (* contents of file A.ml *) end;;
 *)

(* Putting the above on hold because it's a big ass scope creep.
 *
 * Doing this:
 * Keys store sequences of ocaml phrases.
 * Keys are accessible from the interface.
 * Feature: interface stores history (which keys were run when) in a key like .history
 *)

module Store = struct
  let selfstart () = (* load the init stuff in *)
    ()

  let sequence key =
    
      
    

end
                 
