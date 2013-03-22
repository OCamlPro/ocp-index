let info =
  let rec subdirs acc path =
    Array.fold_left
      (fun acc p ->
        let path = Filename.concat path p in
        if Sys.is_directory path then path::subdirs acc path else acc)
      acc
      (Sys.readdir path)
  in
  Info.load
    (subdirs ["/home/louis/opam/lib/ocaml"] "/home/louis/opam/lib/ocaml")

let _ =
  Printf.eprintf "%d definitions loaded.\n%!" (List.length (Info.all info));
  (* let all = Info.all info in *)
  (* List.iter *)
  (*   (fun id -> Printf.printf "%-20s: %s\n%!" (Info.name id) (Info.ty id)) *)
  (*   all *)
  while true do
    let query = read_line () in
    let all = Info.complete info query in
    List.iter
      (fun id -> Printf.printf "%-20s: %s\n%!" (Info.name id) (Info.ty id))
      all
  done
