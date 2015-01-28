(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                      Pierre Chambart (OCamlPro)                        *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

(* Generation of [Flambda] intermediate language code from [Lambda] code.

   The main transformation performed in this pass is closure conversion.
   Function declarations (which may bind one or more variables identifying
   functions, possibly with mutual recursion) are transformed to
   [Fset_of_closures] expressions.  [Fclosure] expressions are then used to
   select a closure for a particular function from a [Fset_of_closures]
   expression.  The [Fset_of_closures] expressions say nothing about the
   actual runtime layout of the closures; this is handled when [Flambda] code
   is translated to [Clambda] code.

   This pass also performs the following transformations.
   - Constant blocks are converted to applications of the [Pmakeblock]
     primitive.
   - [Levent] debugging event nodes are removed and the information within
     them attached to function, method and [raise] calls.
   - Access to global fields of the current compilation unit (of the form
     [Lprim (Pfield _ | Psetfield _, [Lprim (Pgetglobal _, []); ...])])
     are converted to [Pgetglobalfield] and [Psetglobalfield] primitives.
   - Tuplified functions are converted to curried functions and a stub
     function emitted to call the curried version.  For example:
       let rec f (x, y) = f (x + 1, y + 1)
     is transformed to:
       let rec internal_f x y = f (x + 1,y + 1)
       and f (x, y) = internal_f x y  (* [f] is marked as a stub function *)
   - The [Pdirapply] and [Prevapply] application primitives are removed and
     converted to normal [Flambda] application nodes.
   - String constants are lifted to the toplevel to avoid special cases later
     (duplicating them may change the semantics of the program).
*)

open Abstract_identifiers

(* CXR mshinwell for pchambart: Why is the [current_unit_id] not inside
   the type [compilation_unit]?
   pchambart: It is, I removed the argument. In bytecode the compilation unit
     was a dummy argument.
*)
val lambda_to_flambda
   : current_compilation_unit:Symbol.Compilation_unit.t
  (* CR mshinwell for pchambart: Can we remove the ' on this label name?
     pchambart: this reflects the name of the Compilenv.symbol_for_global'
       function. Compilenv.symbol_for_global is the same function
       returning a string (which is probably not well named)
       This argument is not strictly necessary, but it is usefull for
       unit testing witout relying on compilenv. As all those tests
       are currently disabled, this may not be necessary. *)
  -> symbol_for_global':(Ident.t -> Symbol.t)
  -> Lambda.lambda
  -> Expr_id.t Flambda.flambda
