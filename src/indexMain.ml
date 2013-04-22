(**************************************************************************)
(*                                                                        *)
(*  Copyright 2013 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

open Cmdliner

let default_cmd =
  let doc = "Explore the interfaces of installed OCaml libraries." in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ IndexOptions.common_opts)),
  Term.info "ocp-index" ~version:"0.0.1" ~doc

let complete_cmd =
  let doc = "Complete identifiers starting with prefix $(docv)." in
  let t =
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"STRING")
  in
  let print_compl opts query =
    let fmt = Format.std_formatter in
    List.iter
      (fun info ->
         LibIndex.Format.info
           ~colorise:(if opts.IndexOptions.color
                      then LibIndex.Format.color
                      else LibIndex.Format.no_color)
           fmt info;
         Format.pp_print_newline fmt ())
      (LibIndex.complete
         opts.IndexOptions.lib_info
         ~filter:opts.IndexOptions.filter
         query);
    Format.pp_print_flush fmt ()
  in
  let doc = "Print completions to stdout." in
  Term.(pure print_compl $ IndexOptions.common_opts $ t),
  Term.info "complete" ~doc

let type_cmd =
  let doc = "Print the type of OCaml identifier $(docv)." in
  let t =
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"STRING")
  in
  let print_ty opts query =
    try
      let id = LibIndex.get opts.IndexOptions.lib_info query in
      print_endline (LibIndex.Print.ty id)
    with Not_found -> exit 2
  in
  let doc = "Print the type of an identifier." in
  Term.(pure print_ty $ IndexOptions.common_opts $ t),
  Term.info "type" ~doc

let () =
  match Term.eval_choice default_cmd [complete_cmd; type_cmd] with
  | `Error _ -> exit 1
  | _ -> exit 0
