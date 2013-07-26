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
type t = {
  lib_info: LibIndex.t;
  color: bool;
  filter: LibIndex.info -> bool;
  project_root: string option;
}

let cmd_input_line cmd =
  try
    let ic = Unix.open_process_in cmd in
    let r = input_line ic in
    let r =
      let len = String.length r in
      if len>0 && r.[len - 1] = '\r' then String.sub r 0 (len-1) else r
    in
    match Unix.close_process_in ic with
    | Unix.WEXITED 0 -> r
    | _ -> failwith "cmd_input_line"
  with
  | End_of_file | Unix.Unix_error _ | Sys_error _ -> failwith "cmd_input_line"


(* -- configuration file -- *)
type conf_file = { rec_include_dirs: string list;
                   include_dirs: string list;
                   conf_file_dir: string option;
                   (* libs: string list *) }

let string_split char str =
  let rec aux pos =
    try
      let i = String.index_from str pos char in
      String.sub str pos (i - pos) :: aux (succ i)
    with Not_found | Invalid_argument _ ->
        let l = String.length str in
        [ String.sub str pos (l - pos) ]
  in
  aux 0

let get_conf_file ?(path=Sys.getcwd()) () =
  let ( / ) = Filename.concat
  in
  let load conf file =
    try
      let ic = open_in file in
      let contents =
        let b = Buffer.create 512 in
        try while true do
            let s = input_line ic in
            let n = try String.index s '#' with Not_found -> String.length s in
            Buffer.add_substring b s 0 n;
            Buffer.add_char b '\n'
          done; assert false
        with End_of_file -> Buffer.contents b
      in
      let absolute dir =
        (* todo: use +lib to load camllib/lib ? *)
        if Filename.is_relative dir then Filename.dirname file / dir
        else dir
      in
      List.fold_left
        (fun conf s -> match string_split ' ' s with
           | [] | [""] -> conf
           | "dir" :: dirs ->
               let dirs = List.map absolute dirs in
               { conf with include_dirs = dirs @ conf.include_dirs }
           | "rec" :: "dir" :: dirs ->
               let dirs = List.map absolute dirs in
               { conf with rec_include_dirs = dirs @ conf.rec_include_dirs }
           (* | "lib" :: libs ->
                { conf with libs = libs @ conf.libs } *)
           | wrong_key :: _ ->
               let e = Printf.sprintf "wrong configuration key %S" wrong_key
               in raise (Invalid_argument e))
        conf
        (string_split '\n' contents)
    with
    | Sys_error _ ->
        Printf.eprintf
          "ocp-index warning: could not open %S for reading configuration.\n%!"
          file;
        conf
    | Invalid_argument err ->
        Printf.eprintf
          "ocp-index warning: error in configuration file %S:\n%s\n%!"
          file err;
        conf
  in
  let rec find_conf_file path =
    let conf_file_name = ".ocp-index" in
    if Sys.file_exists (path / conf_file_name)
    then Some (path / conf_file_name)
    else
      let path =
        if Filename.is_relative path then Sys.getcwd () / path
        else path
      in
      let parent = Filename.dirname path in
      if parent <> path then find_conf_file parent
      else None
  in
  let conf =
    { rec_include_dirs = []; include_dirs = []; conf_file_dir = None;
      (* libs = [] *) }
  in
  let conf =
    try
      let f = Sys.getenv "HOME" / ".ocp" / "ocp-index.conf" in
      if Sys.file_exists f then load conf f else conf
    with Not_found -> conf
  in
  let conf = match find_conf_file path with
    | Some c -> load {conf with conf_file_dir = Some (Filename.dirname c)} c
    | None -> conf
  in
  conf


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
          (try [cmd_input_line "ocamlc -where"] with Failure _ -> []) @
            (try [cmd_input_line "opam config var lib"] with Failure _ -> [])
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
  let filter : (LibIndex.info -> bool) Term.t =
    let opts = [
      "t", `Types; "types", `Types;
      "v", `Values; "values", `Values;
      "e", `Exceptions; "exceptions", `Exceptions;
      "c", `Constructs; "constructs", `Constructs;
      "m", `Modules; "modules", `Modules;
      "s", `Sigs; "sigs", `Sigs;
    ] in
    let show =
      Arg.(value & opt (list (enum opts)) [] & info ["s";"show"]
             ~doc:"Kinds of elements to show in answers: $(docv) is a \
                   comma-separated list of `$(i,types)', `$(i,values)' and \
                   methods, `$(i,exceptions)', `$(i,constructs)' (record \
                   fields and sum type constructors), `$(i,modules)' and \
                   classes, `$(i,sigs)' (module and class types). The default \
                   is $(b,v,e,c,m)"
             ~docv:"LIST")
    in
    let hide =
      Arg.(value & opt (list (enum opts)) [] & info ["h";"hide"]
             ~doc:"kinds of elements not to show in answers: $(docv) is a \
                   comma-separated list of element kinds (see `--show')"
             ~docv:"LIST")
    in
    Term.(
      pure (fun show hide ->
          let f key default = List.mem key show ||
                              default && not (List.mem key hide)
          in
          let t,v,e,c,m,s =
            f `Types false, f `Values true, f `Exceptions true,
            f `Constructs true, f `Modules true, f `Sigs false
          in
          LibIndex.(fun info -> match info.kind with
              | Type -> t
              | Value | Method _ -> v
              | Exception -> e
              | Field _ | Variant _ -> c
              | Module | Class -> m
              | ModuleType | ClassType -> s))
      $ show $ hide
    )
  in
  let lib_info_and_conf_file : (LibIndex.t * conf_file) Term.t =
    let conf_file = get_conf_file () in
    let dirs =
      let get_all_dirs ocamllib =
        let dirs =
          List.fold_left
            (fun incl d -> if List.mem d incl then incl else d :: incl)
            (LibIndex.unique_subdirs (conf_file.rec_include_dirs @ ocamllib))
            conf_file.include_dirs
        in
        if dirs = [] then
          failwith "Failed to guess OCaml / opam lib dirs. Please use `-I'"
        else dirs
      in
      Term.(pure get_all_dirs $ ocamllib)
    in
    let init dirs opens =
      let info = LibIndex.load dirs in
      List.fold_left (LibIndex.open_module ~cleanup_path:true) info opens,
      conf_file
    in
    Term.(pure init $ dirs $ open_modules)
  in
  Term.(
    pure
      (fun (lib_info,conf_file) color filter ->
         { lib_info; color; filter; project_root = conf_file.conf_file_dir })
    $ lib_info_and_conf_file $ color $ filter
  )
