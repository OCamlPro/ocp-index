let ocamllib =
  let lib =
    try
      let ic = Unix.open_process_in "opam config var lib" in
      let r = input_line ic in
      match Unix.close_process_in ic with
      | Unix.WEXITED 0 -> r
      | _ -> raise Exit
    with Unix.Unix_error _ | Exit -> try
      Sys.getenv "OCAMLLIB"
    with Not_found ->
        prerr_endline
          "Failed to get ocaml lib dir by opam or $OCAMLLIB. Aborting";
        exit 1
  in
  Printf.eprintf "Loading libs from %S.\n%!" lib;
  lib

let info =
  let rec subdirs acc path =
    Array.fold_left
      (fun acc p ->
        let path = Filename.concat path p in
        if Sys.is_directory path then path::subdirs acc path else acc)
      acc
      (Sys.readdir path)
  in
  Info.load (subdirs [ocamllib] ocamllib)

let _ =
  Printf.eprintf "%d definitions loaded.\n%!" (List.length (Info.all info));
  (* let all = Info.all info in *)
  (* List.iter *)
  (*   (fun id -> Printf.printf "%-20s: %s\n%!" (Info.name id) (Info.ty id)) *)
  (*   all *)
  while true do
    let query = read_line () in
    let all = Info.complete info query in
    List.iter (fun id -> print_string (Info.pretty id)) all
  done
