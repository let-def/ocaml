#!/bin/sh

#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*                         Frederic Bour, Tarides                         *
#*                                                                        *
#*   Copyright 2026 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

# Usage: pack-menhirLib.sh ml|mli DIR MODULE...
#
# Recent versions of Menhir install MenhirLib as one file per module.
# This script packs the modules MODULE... of DIR into the implementation
# (ml) or the interface (mli) of a single module, in the order of their
# dependencies.

set -e

usage() {
  echo "Usage: $0 ml|mli DIR MODULE..." >&2
  exit 2
}

test $# -ge 3 || usage
kind=$1
dir=$2
shift 2
case $kind in ml|mli) ;; *) usage;; esac
cd "$dir"

files=
for m in "$@"; do
  for f in "$m.mli" "$m.ml"; do
    if test -r "$f"; then files="$files $f"; fi
  done
done

# Sort the modules: each module is printed after the modules it uses.
sorted=$(
  ocamldep -modules $files |
  sed -e 's/\.mli\{0,1\}:/ /' |
  while read -r m deps; do
    echo "$m $m"
    for d in $deps; do
      for n in "$@"; do
        if test "$d" = "$n"; then echo "$d $m"; fi
      done
    done
  done |
  tsort
)

# Some functors of MenhirLib do not use their parameters
if test "$kind" = ml; then echo '[@@@ocaml.warning "-67"]'; fi

for m in $sorted; do
  if test "$kind" = ml; then
    if test -r "$m.mli" && test -r "$m.ml"; then
      echo "module $m : sig"; cat "$m.mli"; echo "end = struct"
      cat "$m.ml"; echo "end"
    else
      # A module of types and module types only
      echo "module $m = struct"; cat "$m.ml"; echo "end"
    fi
  else
    if test -r "$m.mli"; then
      echo "module $m : sig"; cat "$m.mli"; echo "end"
    else
      echo "module $m : module type of struct"; cat "$m.ml"; echo "end"
    fi
  fi
done
