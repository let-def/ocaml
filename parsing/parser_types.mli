(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         Frederic Bour, Tarides                         *)
(*                                                                        *)
(*   Copyright 2026 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.         *)
(*                                                                        *)
(**************************************************************************)

(* The types of the semantic values of the nonterminals of parser.mly that
   are not defined by the Parsetree. With --inspection, Menhir mentions the
   type of every nonterminal in parser.mli, which cannot see the types
   defined in the header of parser.mly. *)

type let_binding =
  { lb_pattern: Parsetree.pattern;
    lb_expression: Parsetree.expression;
    lb_constraint: Parsetree.value_constraint option;
    lb_is_pun: bool;
    lb_attributes: Parsetree.attributes;
    lb_docs: Docstrings.docs Lazy.t;
    lb_text: Docstrings.text Lazy.t;
    lb_loc: Location.t; }

type let_bindings =
  { lbs_bindings: let_binding list;
    lbs_rec: Asttypes.rec_flag;
    lbs_extension: string Asttypes.loc option }
