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

(* -- common options -- *)
type t = { lib_info: LibIndex.t; color: bool; }

let cmd_input_line cmd =
  try
    let ic = Unix.open_process_in cmd in
    let r = input_line ic in
    match Unix.close_process_in ic with
    | Unix.WEXITED 0 -> r
    | _ -> failwith "cmd_input_line"
  with
  | End_of_file | Unix.Unix_error _ -> failwith "cmd_input_line"

let common_opts : t Term.t =
  let ocamllib : string list Term.t =
    let arg =
      let doc = "OCaml directories to (recursively) load the libraries from, \
                 in addition to OCaml's stdlib directory. \
                 By default, will look for opam's main dir."
      in
      Arg.(value & opt_all (list string) [] & info ["I"] ~docv:"DIRS" ~doc)
    in
    let set_default = function
      | _ :: _ as paths ->
          (try [cmd_input_line "ocamlc -where"] with Failure _ -> []) @
          List.flatten paths
      | [] ->
          let paths =
            (try [cmd_input_line "ocamlc -where"] with Failure _ -> []) @
            (try [cmd_input_line "opam config var lib"] with Failure _ -> [])
          in
          if paths = [] then
            failwith "Failed to guess OCaml / opam lib dirs. Please use `-I'"
          else paths
    in
    Term.(pure set_default $ arg)
  in
  let color : bool Term.t =
    let arg =
      let choices = Arg.enum [ "always", `Always;
                               "never", `Never;
                               "auto", `Auto; ]
      in
      let doc =
        "Colorize the output. $(docv) is either `always', `never' or `auto'."
      in
      Arg.(
        last & opt_all choices [`Auto] & info ["c";"color"] ~docv:"WHEN" ~doc
      )
    in
    let to_bool = function
      | `Always -> true
      | `Never -> false
      | `Auto -> Unix.isatty Unix.stdout
    in
    Term.(pure to_bool $ arg)
  in
  let open_modules : string list list Term.t =
    let arg =
      let doc =
        "Consider the given (comma-separated list of) modules are opened \
         for lookup."
      in
      Arg.(
        value & opt_all (list ~sep:',' (list ~sep:'.' string)) [] &
        info ["O";"open"] ~docv:"MODULES" ~doc
      )
    in
    Term.(pure List.flatten $ arg)
  in
  let lib_info : LibIndex.t Term.t =
    let dirs =
      Term.(
        pure LibIndex.unique_subdirs $ ocamllib
      )
    in
    let init dirs opens =
      let info = LibIndex.load dirs in
      List.fold_left LibIndex.open_module info opens
    in
    Term.(pure init $ dirs $ open_modules)
  in
  Term.(
    pure (fun lib_info color -> { lib_info; color; })
    $ lib_info $ color
  )
