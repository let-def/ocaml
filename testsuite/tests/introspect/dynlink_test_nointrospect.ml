(* TEST
   readonly_files = "printval.ml";
   no-introspect;
   {
     native-dynlink;
     native-compiler;
     setup-ocamlopt.opt-build-env;
     script = "sh ${test_source_directory}/dynlink_test.sh dynlink_test_nointrospect.ml";
     script;
     check-program-output;
   }{
     setup-ocamlc.byte-build-env;
     script = "sh ${test_source_directory}/dynlink_test_bytecode.sh dynlink_test_nointrospect.ml";
     script;
     check-program-output;
   }
*)

(* printval.ml uses Gc: make sure it is linked in the main program *)
let () = ignore (Gc.minor : unit -> unit)

let () = Dynlink.loadfile (if Dynlink.is_native then "printval.cmxs" else "printval.cmo")
