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
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The messages of the syntax errors, compiled by LRgrep from
    parse_errors.lrgrep.

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

val error_messages :
  _ Parser.MenhirInterpreter.env ->
  Parser.token * Lexing.position * Lexing.position ->
  (string * (Location.t * string) list) option
(** [error_messages env token] explains the failure of the parser in the
    state [env] on [token]: a message, and secondary messages at other
    locations. Code is quoted between backquotes, as in Markdown. *)
