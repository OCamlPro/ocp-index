open Cmdliner

(* -- common options -- *)
type common_opts = { lib_info: Info.t; color: bool; }

let rec subdirs acc path =
  Array.fold_left
    (fun acc p ->
      let path = Filename.concat path p in
      if Sys.is_directory path then subdirs acc path else acc)
    (path::acc)
    (Sys.readdir path)

let remove_dups l =
  let rec aux = function
    | a::(b::_ as r) when a = b -> aux r
    | a::r -> a :: aux r
    | [] -> []
  in
  aux (List.sort compare l)

let cmd_input_line cmd =
  try
    let ic = Unix.open_process_in cmd in
    let r = input_line ic in
    match Unix.close_process_in ic with
    | Unix.WEXITED 0 -> r
    | _ -> failwith "cmd_input_line"
  with
  | End_of_file | Unix.Unix_error _ -> failwith "cmd_input_line"

let common_opts : common_opts Term.t =
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
  let lib_info : Info.t Term.t =
    let dirs =
      Term.(
        pure (fun d -> remove_dups (List.fold_left subdirs [] d))
        $ ocamllib
      )
    in
    Term.(pure Info.load $ dirs)
  in
  Term.(
    pure (fun lib_info color -> { lib_info; color; })
    $ lib_info $ color
  )

(* -- Commands -- *)

let default_cmd =
  let doc = "Explore the interfaces of installed OCaml libraries." in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ common_opts)),
  Term.info "ocp-index" ~version:"0.0.1" ~doc

let complete_cmd =
  let doc = "Complete identifiers starting with prefix $(docv)." in
  let t =
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"STRING")
  in
  let print_compl opts query =
    let fmt = Format.std_formatter in
    List.iter
      (fun id ->
        Info.format_id ~color:opts.color fmt id;
        Format.pp_print_newline fmt ())
      (Info.complete opts.lib_info query);
    Format.pp_print_flush fmt ()
  in
  let doc = "Print completions to stdout." in
  Term.(pure print_compl $ common_opts $ t),
  Term.info "complete" ~doc

let type_cmd =
  let doc = "Print the type of OCaml identifier $(docv)." in
  let t =
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"STRING")
  in
  let print_ty opts query =
    try
      let id = Info.get opts.lib_info query in
      print_endline (Info.ty id)
    with Not_found -> exit 2
  in
  let doc = "Print the type of an identifier." in
  Term.(pure print_ty $ common_opts $ t),
  Term.info "type" ~doc

let () =
  match Term.eval_choice default_cmd [complete_cmd; type_cmd] with
  | `Error _ -> exit 1
  | _ -> exit 0
