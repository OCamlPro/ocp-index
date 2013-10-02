(**************************************************************************)
(*                                                                        *)
(*  Copyright 2013 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the Lesser GNU Public License version 3.0.                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  Lesser GNU General Public License for more details.                   *)
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
    let ic = Unix.open_process_in (cmd ^ " 2>/dev/null") in
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

let build_roots = (* by increasing order of priority *)
  [ ".svn"; "_darcs"; ".hg"; ".git";
    "jengaroot.ml"; "omakeroot"; "_build"; "_obuild" ]

let find_build_dir path =
  let ( / ) = Filename.concat in
  let files = Sys.readdir path in
  let _, root =
    let rec memsuffix x = function
      | a::r -> if x = a then Some r else memsuffix x r
      | [] -> None
    in
    Array.fold_left (fun (roots,found) f ->
        match memsuffix f roots with
        | None -> roots, found
        | Some roots -> roots, Some f)
      (build_roots, None) files
  in
  match root with
  | None -> None
  | Some ("_obuild" | "_build" as dir) -> Some (path / dir)
  | Some _ -> Some path

let project_root ?(path=Sys.getcwd()) () =
  let ( / ) = Filename.concat in
  let home = try Sys.getenv "HOME" with Not_found -> "" in
  let path =
    if Filename.is_relative path then Sys.getcwd () / path
    else path
  in
  let rec find path =
    match find_build_dir path with
    | None ->
        let parent = Filename.dirname path in
        if path = parent || path = home then None
        else find parent
    | Some build -> Some (path, build)
  in
  match find path with
  | None -> None, None
  | Some (root, build) -> Some root, Some build

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
  let open_modules : (string list list * string list list) Term.t =
    let arg_open =
      let doc =
        "Consider the given (comma-separated list of) modules are opened \
         for lookup."
      in
      Arg.(
        value & opt_all (list ~sep:',' (list ~sep:'.' string)) [] &
        info ["O";"open"] ~docv:"MODULES" ~doc
      )
    in
    let arg_full_open =
      let doc =
        "Like `--open', but if the .cmt is available, load even the \
         elements that would be restricted by the interface. Useful for \
         the current file."
      in
      Arg.(
        value & opt_all (list ~sep:',' (list ~sep:'.' string)) [] &
        info ["F";"full-open"] ~docv:"MODULES" ~doc
      )
    in
    Term.(pure (fun o fo -> List.flatten o, List.flatten fo)
          $ arg_open $ arg_full_open)
  in
  let filter : (LibIndex.info -> bool) Term.t =
    let opts = [
      "t", `Types; "types", `Types;
      "v", `Values; "values", `Values;
      "e", `Exceptions; "exceptions", `Exceptions;
      "c", `Constructs; "constructs", `Constructs;
      "m", `Modules; "modules", `Modules;
      "s", `Sigs; "sigs", `Sigs;
      "k", `Keywords; "keywords", `Keywords;
    ] in
    let show =
      Arg.(value & opt (list (enum opts)) [] & info ["s";"show"]
             ~doc:"Kinds of elements to show in answers: $(docv) is a \
                   comma-separated list of `$(i,types)', `$(i,values)' and \
                   methods, `$(i,exceptions)', `$(i,constructs)' (record \
                   fields and sum type constructors), `$(i,modules)' and \
                   classes, `$(i,sigs)' (module and class types), \
                   `$(i,keywords)'. The default \
                   is $(v,e,c,m,k)"
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
          let t,v,e,c,m,s,k =
            f `Types false, f `Values true, f `Exceptions true,
            f `Constructs true, f `Modules true, f `Sigs false,
            f `Keyword true
          in
          LibIndex.(fun info -> match info.kind with
              | Type -> t
              | Value | Method _ -> v
              | Exception -> e
              | Field _ | Variant _ -> c
              | Module | Class -> m
              | ModuleType | ClassType -> s
              | Keyword -> k))
      $ show $ hide
    )
  in
  let project_dirs : (string option * string option) Term.t =
    let root =
      let doc = "Set the current project root (default: try to guess)" in
      Arg.(value & opt (some string) None & info ["root"] ~docv:"DIR" ~doc)
    in
    let build =
      let doc = "Set the current project build dir (default: try to guess)" in
      Arg.(value & opt (some string) None & info ["build"] ~docv:"DIR" ~doc)
    in
    let default root build =
      match root, build with
      | None, None -> project_root ()
      | Some r as root, None -> root, find_build_dir r
      | None, (Some b as build) -> Some (Filename.dirname b), build
      | ds -> ds
    in
    Term.(pure default $ root $ build)
  in
  let context : (string option * int option * int option) option Term.t =
    let doc = "Will analyse the context at given FILE[:LINE,COL] to \
               give appropriate answers w.r.t open modules, etc. \
               You can specify just `:' to read from stdin" in
    let filepos_converter =
      (fun str -> match fst (Arg.(pair ~sep:':' string string)) str with
         | `Ok ("","") -> `Ok (None, None, None)
         | `Ok (file,"") ->
             (match (fst Arg.non_dir_file) file with
              | `Ok file -> `Ok (Some file, None, None)
              | `Error e -> `Error e)
         | `Ok (file,pos) ->
             (match (fst Arg.non_dir_file) file,
                    (fst (Arg.list Arg.int)) pos with
              | `Ok file, `Ok [line; col] ->
                  `Ok (Some file, Some line, Some col)
              | `Ok file, `Ok [line] ->
                  `Ok (Some file, Some line, None)
              | `Error e, _ | _, `Error e -> `Error e
              | _ ->
                  `Error
                    (Printf.sprintf "Wrong file position %S, should be \
                                     <line> or <line>,<col>" pos))
         | `Error _ ->
             (match (fst Arg.non_dir_file) str with
              | `Ok file -> `Ok (Some file, None, None)
              | `Error e -> `Error e)),
      (fun fmt (file,line,col) ->
         let opt f fmt = function None -> () | Some x -> f fmt x in
         Format.fprintf fmt "%a%s%a%a"
           (opt Format.pp_print_string) file
           (if file = None || line <> None then ":" else "")
           (opt Format.pp_print_int) line
           (opt Format.pp_print_int) col)
    in
    Arg.(value & opt (some filepos_converter) None
         & info ["context"] ~docv:"FILE" ~doc)
  in
  let lib_info ocamllib (_root,build) (opens,full_opens) context =
    let dirs = match build with
      | None -> ocamllib
      | Some d -> LibIndex.Misc.unique_subdirs (d :: ocamllib)
    in
    if dirs = [] then
      failwith "Failed to guess OCaml / opam lib dirs. Please use `-I'";
    let info = LibIndex.load dirs in
    let info =
      List.fold_left (LibIndex.open_module ~cleanup_path:true) info opens
    in
    let info =
      List.fold_left (LibIndex.fully_open_module ~cleanup_path:true)
        info full_opens
    in
    let info = match context with
      | None -> info
      | Some (file,line,column) ->
          let chan = match file with Some f -> open_in f | None -> stdin in
          let scope = IndexScope.read ?line ?column chan in
          let () = match file with Some _ -> close_in chan | None -> () in
          let info =
            List.fold_left (fun info -> function
                | IndexScope.Open path ->
                    LibIndex.open_module ~cleanup_path:true info path
                | IndexScope.Alias (name,path) ->
                    LibIndex.alias ~cleanup_path:true info path [name])
              info (IndexScope.to_list scope)
          in
          info
    in
    info
  in
  Term.(
    pure
      (fun ocamllib project_dirs opens color filter context ->
         { lib_info = lib_info ocamllib project_dirs opens context;
           color; filter; project_root = fst project_dirs })
    $ ocamllib $ project_dirs $ open_modules $ color $ filter $ context
  )
