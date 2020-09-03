(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let cmi_magic_number = "Caml1999I025"
and ast_impl_magic_number = "Caml1999M025"
and ast_intf_magic_number = "Caml1999N025"
and cmt_magic_number = "Caml1999T025"

(* placeholders *)

let safe_string = false
let with_flambda_invariants = false
let standard_library = ""
let architecture = ""
let default_safe_string = false
let afl_instrument = false
let flambda = false
let print_config _ = ()
let config_var _ = None
let as_has_debug_prefix_map = false
