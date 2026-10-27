/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Xavier Leroy and Damien Doligez, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include "caml/config.h"
#ifndef _WIN32
#include <unistd.h>
#else
#include <io.h>
#endif
#include "caml/alloc.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/platform.h"
#include "caml/intext.h"
#include "caml/startup.h"

#ifdef NATIVE_CODE
# include "caml/stack.h"
#endif

static value compiler_block_descs = Val_unit;
static atomic_int compiler_block_descs_ready = 0;
static caml_plat_mutex compiler_block_descs_lock = CAML_PLAT_MUTEX_INITIALIZER;

CAMLprim value caml_compiler_block_descs(value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(r);

  if (!atomic_load_acquire(&compiler_block_descs_ready)) {
    /* Allocate outside of the lock: allocation can trigger a GC */
    r = caml_alloc_small(1, 0);
    Field(r, 0) = Val_emptylist;

    caml_plat_lock_non_blocking(&compiler_block_descs_lock);
    if (!atomic_load_relaxed(&compiler_block_descs_ready)) {
      compiler_block_descs = r;
      caml_register_generational_global_root(&compiler_block_descs);
      atomic_store_release(&compiler_block_descs_ready, 1);
    }
    caml_plat_unlock(&compiler_block_descs_lock);
  }

  CAMLreturn(compiler_block_descs);
}

#ifdef NATIVE_CODE

CAMLprim value caml_read_bdsc_section(value unit)
{
  return caml_input_value_from_block(caml_globals_block_descs, INT_MAX);
}

#else

CAMLprim value caml_read_bdsc_section(value unit)
{
  (void)unit;
  CAMLparam0();
  CAMLlocal1(library);
  int fd;
  char_os *exec_name;
  struct channel *chan;
  struct exec_trailer trail;

  library = Val_unit;

  if (caml_params->cds_file == NULL && caml_byte_program_mode == EMBEDDED)
    CAMLreturn(Val_unit);

  if (caml_params->cds_file != NULL)
    exec_name = (char_os*) caml_params->cds_file;
  else
    exec_name = (char_os*) caml_params->exe_name;

  fd = caml_attempt_open(&exec_name, &trail, 1);
  if (fd < 0)
    CAMLreturn(Val_unit);

  caml_read_section_descriptors(fd, &trail);
  if (caml_seek_optional_section(fd, &trail, "BDSC") != -1) {
    caml_stat_free(trail.section);
    /* The channel owns [fd] from now on */
    chan = caml_open_descriptor_in(fd);
    library = caml_input_val(chan);
    caml_close_channel(chan);
  } else {
    caml_stat_free(trail.section);
    close(fd);
  }

  CAMLreturn(library);
}

#endif
