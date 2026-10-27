(* TEST
 (* The opaque printer shows more with introspection *)
 no-introspect;
 toplevel;
*)

List.fold_left;;
#trace List.fold_left;;
0;;
List.fold_left (+) 0 [1;2;3];;
