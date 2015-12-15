(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

type result

(** [inconstants_on_program] with [for_clambda = true] finds those variables
    and set-of-closures identifiers that cannot be compiled to constants by
    [Flambda_to_clambda].

    When [for_clambda] is false, field accesses to a constant are
    considered constant.
*)
val inconstants_on_program
   : for_clambda:bool
  -> compilation_unit:Compilation_unit.t
  -> Flambda.program
  -> result

(** [variable var res] returns [true] if [var] is marked as inconstant
    in [res]. *)
val variable : Variable.t -> result -> bool

(** [closure cl res] returns [true] if [cl] is marked as inconstant
    in [res]. *)
val closure : Set_of_closures_id.t -> result -> bool
