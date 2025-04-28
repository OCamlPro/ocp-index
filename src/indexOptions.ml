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

type filter_kind = {
    t : bool ;
    v : bool ;
    e : bool ;
    c : bool ;
    m : bool ;
    s : bool ;
    k : bool ;
  }

type t = {
  lib_info: LibIndex.t;
  color: bool;
  mutable filter: filter_kind ;
  project_root: string option;
}

let filter opt info =
  let open LibIndex in
  let kinds = opt.filter in
  match info.kind with
  | OpenType
  | Type -> kinds.t
  | Value | Method _ -> kinds.v
  | Exception -> kinds.e
  | Field _ | Variant _ -> kinds.c
  | Module | Class -> kinds.m
  | ModuleType | ClassType -> kinds.s
  | Keyword -> kinds.k

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



let default_filter = [ `V ; `E ; `C ; `M ; `K ]

let filter_to_string l =
  let pp = function
    | `V -> "v" | `E -> "e" | `C -> "c"
    | `M -> "m" | `K -> "k" | `S -> "s"
    | `T -> "t"
  in
  String.concat "," (List.map pp l)

let common_opts ?(default_filter = default_filter) () : t Term.t =
  let ocamllib : string list Term.t =
    let no_stdlib : bool Term.t =
      let doc = "Don't include the OCaml standard library directory \
                 (`ocamlc -where`) in lookups." in
      Arg.(value & flag & info ["no-stdlib"] ~doc);
    in
    let no_opamlib : bool Term.t =
      let doc = "Don't include the OPAM library directory (`opam var lib`) \
                 in lookups." in
      Arg.(value & flag & info ["no-opamlib"] ~doc);
    in
    let arg =
      let doc = "OCaml directories to (recursively) load the libraries from." in
      Arg.(value & opt_all (list string) [] & info ["I"] ~docv:"DIRS" ~doc)
    in
    let set_default no_stdlib no_opamlib includes =
      let paths = List.flatten includes in
      let paths =
        if no_opamlib then paths else
          try cmd_input_line "opam config var lib" :: paths
          with Failure _ -> paths
      in
      if no_stdlib then paths else
        try cmd_input_line "ocamlc -where" :: paths with Failure _ -> paths
    in
    Term.(const set_default $ no_stdlib $ no_opamlib $ arg)
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
    Term.(const to_bool $ arg)
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
    Term.(const (fun o fo -> List.flatten o, List.flatten fo)
          $ arg_open $ arg_full_open)
  in
  let filter : filter_kind Term.t =
    let opts = [
      "t", `T; "types", `T;
      "v", `V; "values", `V;
      "e", `E; "exceptions", `E;
      "c", `C; "constructs", `C;
      "m", `M; "modules", `M;
      "s", `S; "sigs", `S;
      "k", `K; "keywords", `K;
    ] in
    let show =
      Arg.(value & opt (list (enum opts)) [] & info ["s";"show"]
             ~doc:(
               Printf.sprintf
                 "Kinds of elements to show in answers: $(docv) is a \
                  comma-separated list of `$(i,types)', `$(i,values)' and \
                  methods, `$(i,exceptions)', `$(i,constructs)' (record \
                  fields and sum type constructors), `$(i,modules)' and \
                  classes, `$(i,sigs)' (module and class types), \
                  `$(i,keywords)'. The default \
                  is $(b,%s)" (filter_to_string default_filter)
             )
             ~docv:"LIST")
    in
    let hide =
      Arg.(value & opt (list (enum opts)) [] & info ["h";"hide"]
             ~doc:"kinds of elements not to show in answers: $(docv) is a \
                   comma-separated list of element kinds (see `--show')"
             ~docv:"LIST")
    in
    Term.(
      const (fun show hide ->
          let f key = List.mem key show ||
                      List.mem key default_filter &&
                      not (List.mem key hide)
          in
          let t,v,e,c,m,s,k =
            f `T, f `V, f `E,
            f `C, f `M, f `S,
            f `K
          in
          { t ; v ; e ; c ; m ; s ; k })
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
      | None, None -> IndexMisc.project_root ()
      | Some r as root, None -> root, IndexMisc.find_build_dir r
      | None, (Some b as build) -> Some (Filename.dirname b), build
      | ds -> ds
    in
    Term.(const default $ root $ build)
  in
  let context : (bool * string option * int option * int option) option Term.t =
    let doc = "Will analyse the context at given FILE[:LINE[,COL]] to \
               give appropriate answers w.r.t open modules, etc. \
               To read from stdin, specify just `:', or alternatively \
               `FILENAME:-' to still specify a filename which will be used to \
               detect e.g. the owning library scope."
    in
    let filepos_converter =
      (fun str -> match fst (Arg.(pair ~sep:':' string string)) str with
         | `Ok ("","") -> `Ok (true, None, None, None)
         | `Ok (file,"") ->
             (match (fst Arg.non_dir_file) file with
              | `Ok file -> `Ok (false, Some file, None, None)
              | `Error e -> `Error e)
         | `Ok (file,"-") ->
             (match (fst Arg.string) file with
              | `Ok file ->
                  `Ok (true, Some file, None, None)
              | `Error e -> `Error e)
         | `Ok (file,pos) ->
             (match (fst Arg.non_dir_file) file,
                    (fst (Arg.list Arg.int)) pos with
              | `Ok file, `Ok [line; col] ->
                  `Ok (false, Some file, Some line, Some col)
              | `Ok file, `Ok [line] ->
                  `Ok (false, Some file, Some line, None)
              | `Error e, _ | _, `Error e -> `Error e
              | _ ->
                  `Error
                    (Printf.sprintf "Wrong file position %S, should be \
                                     <line> or <line>,<col>" pos))
         | `Error _ ->
             (match (fst Arg.non_dir_file) str with
              | `Ok file -> `Ok (false, Some file, None, None)
              | `Error e -> `Error e)),
      (fun fmt (use_stdin,file,line,col) ->
         let opt f fmt = function None -> () | Some x -> f fmt x in
         Format.fprintf fmt "%a%s%s%a%a"
           (opt Format.pp_print_string) file
           (if file = None || line <> None then ":" else "")
           (if use_stdin && file <> None then "-" else "")
           (opt Format.pp_print_int) line
           (opt Format.pp_print_int) col)
    in
    Arg.(value & opt (some filepos_converter) None
         & info ["context"] ~docv:"FILEPOS" ~doc)
  in
  let lib_info ocamllib (_root,build) (opens,full_opens) context =
    IndexMisc.debug "Using project root at %s\n\
                    \      build dir at %s\n\
                    \      libdir at %s\n"
      (match _root with Some d -> d | None -> "<none>")
      (match build with Some d -> d | None -> "<none>")
      (String.concat ", " ocamllib);
    let dirs = match build with
      | None -> ocamllib
      | Some d -> d :: ocamllib
    in
    if dirs = [] then
      failwith "Failed to guess OCaml / opam lib dirs. Please use `-I'";
    let dirs =
      let skip d = match d.[0] with
        | '_' -> true
        | '.' when not (LibIndex.Misc.file_extension d = "objs") -> true
        | _ -> false
      in
      LibIndex.Misc.unique_subdirs ~skip dirs
    in
    let info = LibIndex.load ~qualify:true dirs in
    let info = match context with
      | None -> info
      | Some (use_stdin,file,line,column) ->
          let chan = match use_stdin, file with
              | true, _ -> stdin
              | false, Some f -> open_in f
              | false, None -> assert false
          in
          let scope = IndexScope.read ?line ?column chan in
          let () = match use_stdin, file with
            | false, Some _ -> close_in chan
            | _ -> ()
          in
          let context_opens =
            match file with
            | None -> []
            | Some f ->
                match IndexScope.from_dot_merlin (Filename.dirname f) with
                | _::_ as opens -> opens
                | [] -> match Dunextract.get_libname f with
                  | Some libname -> [Open [String.capitalize_ascii libname]]
                  | None -> []
          in
          let info =
            List.fold_left (fun info -> function
                | IndexScope.Open path ->
                    LibIndex.open_module ~cleanup_path:true info path
                | IndexScope.Alias (name,path) ->
                    LibIndex.alias ~cleanup_path:true info path [name])
              info (context_opens @ IndexScope.to_list scope)
          in
          info
    in
    let info =
      List.fold_left (LibIndex.open_module ~cleanup_path:true) info opens
    in
    let info =
      List.fold_left (LibIndex.fully_open_module ~qualify:true ~cleanup_path:true)
        info full_opens
    in
    info
  in
  Term.(
    const
      (fun ocamllib project_dirs opens color filter context ->
         { lib_info = lib_info ocamllib project_dirs opens context;
           color; filter; project_root = fst project_dirs })
    $ ocamllib $ project_dirs $ open_modules $ color $ filter $ context
  )
