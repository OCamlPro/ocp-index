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

open Approx_tokens

let curpath_update cp tok =
  let id = function
    | DOT -> `Dot
    | UIDENT s -> `Uid s
    | LIDENT s
    | INFIXOP0 s | INFIXOP1 s | INFIXOP2 s | INFIXOP3 s | INFIXOP4 s -> `Lid s
    | AMPERSAND -> `Lid "&"
    | AMPERAMPER -> `Lid "&&"
    | BARBAR -> `Lid "||"
    | COLONEQUAL -> `Lid ":="
    | MINUS -> `Lid "-"
    | MINUSDOT -> `Lid "-."
    | OR -> `Lid "or"
    | PLUS -> `Lid "+"
    | PLUSDOT -> `Lid "+."
    | QUESTIONQUESTION -> `Lid "??"
    | STAR -> `Lid "*"
    | BANG -> `Lid "!"
    | _ -> `None
  in
  match (id tok, cp) with
  | `None, _ -> []
  | `Dot, `Uid _ :: _ -> `Dot :: cp
  | `Dot, _ -> []
  | (`Uid _ | `Lid _) as c, `Dot :: cp -> c :: cp
  | (`Uid _ | `Lid _) as c, _ -> [c]
  | _ -> []

let path_of_curpath = function
  | `Dot :: _ -> []
  | cp -> List.rev_map (function `Uid s | `Lid s -> s | `Dot -> assert false) cp

let rec list_rm_pfx pfx l = match pfx, l with
  | px :: pr, x :: r when px = x -> list_rm_pfx pr r
  | [], l -> Some l
  | _ -> None

(* Returns the list of paths that could match [path] within an env *)
let rec possible_paths path envs =
  match envs with
  | IndexScope.Open p :: envs ->
      (match list_rm_pfx p path with
       | Some subpath -> possible_paths subpath envs
       | None -> []) @
      possible_paths path envs
  | IndexScope.Alias (id, p) :: envs ->
      (match list_rm_pfx p path with
       | Some subpath -> possible_paths (id :: subpath) envs
       | None -> []) @
      possible_paths path envs
  | [] -> [path]

let grep_file path f ch =
  let modname =
    let s = Filename.basename (Filename.chop_extension f) in
    s.[0] <- Char.uppercase s.[0];
    s
  in
  let f (curpath, lookfor, last_scope, acc) scope tok pos =
    let lookfor =
      if scope == last_scope then lookfor
      else possible_paths path (IndexScope.to_list scope)
    in
    let curpath = curpath_update curpath tok in
    if curpath <> [] && List.hd curpath <> `Dot &&
       List.mem (path_of_curpath curpath) lookfor
    then
      curpath, lookfor, scope, pos :: acc
    else curpath, lookfor, scope, acc
  in
  let _, _, _, matches =
    IndexScope.fold f ([], [], IndexScope.empty, [])
      ~init:[IndexScope.Open ["Pervasives"]; IndexScope.Open [modname]]
      ch
  in
  matches

let rec rmdups = function
  | x::(y::_ as l) when x = y -> rmdups l
  | x::r -> x :: rmdups r
  | [] -> []

let lines_of_file ch lines =
  let rec aux curline = function
    | [] -> []
    | l::r when l = curline ->
        let txt = input_line ch in
        (l, txt) :: aux (curline+1) r
    | lines ->
        while input_char ch <> '\n' do () done;
        aux (curline + 1) lines
  in
  seek_in ch 0;
  aux 1 (rmdups lines)

let files_of_dir dirs =
  List.fold_left (fun acc dir ->
      let files = Array.to_list (Sys.readdir dir) in
      List.map (Filename.concat dir)
        (List.filter (fun f ->
             Filename.check_suffix f ".ml" ||
             Filename.check_suffix f ".mli")
            files)
      @ acc)
    [] (IndexMisc.unique_subdirs dirs)

open Cmdliner

let path =
  let doc = "Fully qualified ident to search for \
             (eg. `List.map', `Set.Make.add', ...)" in
  let arg =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"ID" ~doc)
  in
  let path s = IndexMisc.(key_to_modpath (string_to_key s)) in
  Term.(pure path $ arg)

let files =
  let doc = "Files or directories to search into" in
  let arg = Arg.(value & pos_right 0 file [] & info [] ~docv:"FILES" ~doc) in
  let get_files = function
    | [] -> files_of_dir [Filename.current_dir_name]
    | fs ->
        let dirs, files = List.partition Sys.is_directory fs in
        files @ files_of_dir dirs
  in
  Term.(pure get_files $ arg)

let color =
  let arg =
    let choices = Arg.enum
        [ "always", `Always;
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

let grep path files color =
  let print_line =
    if not color then
      fun file _ (l,txt) -> Printf.printf "%s:%d:%s\n" file l txt
    else
      fun file matches (l,txt) ->
        Printf.printf "%s\027[36m:\027[m%d\027[36m:\027[m" file l;
        let m = List.rev (List.filter (fun (l1,_,_) -> l = l1) matches) in
        let len = String.length txt in
        let offs =
          List.fold_left (fun offs (_,col,toklen) ->
              print_string (String.sub txt offs (col - offs));
              print_string "\027[1;31m";
              print_string (String.sub txt col (min (len - col) toklen));
              print_string "\027[m";
              min len (col + toklen)) 0 m
        in
        print_endline (String.sub txt offs (String.length txt - offs))
  in
  List.iter (fun file ->
      try
        let ch = open_in file in
        let matches = grep_file path file ch in
        let lines = lines_of_file ch (List.rev_map (fun (l,_,_) -> l) matches) in
        List.iter (print_line file matches) lines;
        close_in ch
      with e ->
        Printf.eprintf "%s: %s\n%!" file (Printexc.to_string e)
    )
    files

let () =
  let doc = "Locates instances of a given OCaml ident in source files, \
             handling (local) opens, module, etc." in
  let man = [
    `S "BUGS";
    `P "Current version won't handle shadowing and different kinds of idents, \
        therefore you can get false positive when this happens";
  ]
  in
  match
    Term.eval
      (Term.(pure grep $ path $ files $ color),
       Term.info "ocp-grep" ~version:"0.1" ~doc ~man)
  with
  | `Error _ -> exit 1
  | _ -> exit 0

(* idea: single utility to color parts of source with syntactic context:
   pattern, expr, type, topexpr, module, record ...
   Could be used for better completion, analysis etc.*)

