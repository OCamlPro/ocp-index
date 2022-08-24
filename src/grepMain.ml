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

module Grep : sig

  (* * [ident path, filename, input channel, [line, column, length]]
      Finds occurence of given ident in an opened file.
      Results are in reverse order. *)
  val ident: string list -> string -> in_channel -> (int * int * int) list

  (* * [regex, filename, input channel, [line, column, length]]
      Finds matches of a regexp in the strings of an opened ocaml file.
      Results are in reverse order. *)
  val strings_re: Re.re -> string -> in_channel -> (int * int * int) list

end = struct
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

  let ident path f ch =
    let modname =
      let s =
        Filename.basename
          (try Filename.chop_extension f with Invalid_argument _ -> f)
      in
#if OCAML_VERSION >= (4,03,0)
      String.mapi (function 0 -> Char.uppercase_ascii | _ -> fun x -> x) s
#elif OCAML_VERSION >= (4,02,0)
      String.mapi (function 0 -> Char.uppercase | _ -> fun x -> x) s
#else
      s.[0] <- Char.uppercase s.[0];
      s
#endif
    in
    let modpath =
      match Dunextract.get_libname f with
      | Some libname -> [String.capitalize_ascii libname; modname]
      | None -> [modname]
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
        ~init:[IndexScope.Open ["Pervasives"]; IndexScope.Open ["Stdlib"]; IndexScope.Open modpath]
        ch
    in
    matches

  let strings_re re _f ch =
    let match_re s =
      let rec aux acc pos =
        let ofs =
          try Some (Re.Group.offset (Re.exec ~pos re s) 0) with Not_found -> None
        in
        match ofs with
        | None -> acc
        | Some ((_,pos) as m) -> aux (m :: acc) pos
      in
      aux [] 0
    in
    let ns = Nstream.of_channel ch in
    let rec aux ns matches =
      match Nstream.next ns with
      | Some ({Nstream.token=STRING s} as tok, ns) ->
          let pos = Pos.Region.fst tok.Nstream.region in
          let orig_line, orig_col =
            Lexing.(pos.pos_lnum, pos.pos_cnum - pos.pos_bol + 1)
          in
          let s_matches =
            List.rev_map
              (fun (ofs,ofs_end) -> orig_line, orig_col + ofs, ofs_end - ofs)
              (match_re s)
          in
          aux ns (List.rev_append s_matches matches)
      | Some (_, ns) -> aux ns matches
      | None -> matches
    in
    aux ns []
end

module Args = struct
  open Cmdliner

  let files_of_dir dirs =
    let skip d = match d.[0] with
      | '_' | '.' -> true
      | _ -> false
    in
    List.fold_left (fun acc dir ->
        let files = Array.to_list (Sys.readdir dir) in
        List.map (Filename.concat dir)
          (List.filter (fun f ->
               (Filename.check_suffix f ".ml" ||
                Filename.check_suffix f ".mli" ||
                Filename.check_suffix f ".mll" ||
                Filename.check_suffix f ".mly") &&
               let c = f.[0] in c <> '.' && c <> '#')
              files)
        @ acc)
      [] (IndexMisc.unique_subdirs ~skip dirs)

  let pattern =
    let doc = "Fully qualified ident to search for \
               (eg. `List.map', `Set.Make.add', ...), \
               or string with option `-s'." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"ID" ~doc)

  let files =
    let doc = "Files or directories to search into. By default, searches for \
               project root" in
    let arg = Arg.(value & pos_right 0 file [] & info [] ~docv:"FILES" ~doc) in
    let get_files = function
      | [] ->
          let dir = match IndexMisc.project_root () with
            | Some d, _ -> d
            | None, _ -> Filename.current_dir_name
          in files_of_dir [dir]
      | fs ->
          let dirs, files = List.partition Sys.is_directory fs in
          files @ files_of_dir dirs
    in
    Term.(const get_files $ arg)

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
    Term.(const to_bool $ arg)

  let strings =
    let doc = "Search strings within strings in the source, \
               instead of searching code"
    in
    Arg.(value & flag & info ["s";"strings"] ~doc)

  let regexp =
    let doc = "Like `--strings', but search a POSIX regular expression." in
    Arg.(value & flag & info ["e";"regexp-strings"] ~doc)
end

let rec spaceleft str i =
  let notspace = function ' ' | '\t' | '\n' -> false | _ -> true in
  if i >= String.length str || notspace str.[i] then i
  else spaceleft str (i+1)

let lines_of_file ch matches =
  let rec seek l0 col0 (curline, txt, n, len as cur) matches acc =
    (* Seeks a match. The column may possibly overflow the line for multi-line
       strings *)
    let txtlen = String.length txt in
    let esc_nl = txtlen > 0 && txt.[txtlen - 1] = '\\' in
    let overflow = n - txtlen + (if esc_nl then 1 else 0) in
    if overflow < 0 then
      let acc = cur :: acc in
      match matches with
      | (l1, col1, len1)::r ->
          if l1 = l0 then seek l0 col1 (curline, txt, (n + col1 - col0), len1) r acc
          else if l1 = curline then seek l1 col1 (curline, txt, col1, len1) r acc
          else matches, curline, acc
      | matches -> matches, curline, acc
    else
      let txt = input_line ch and curline = curline + 1 in
      let n =
        if esc_nl then overflow + spaceleft txt 0
        else overflow - 1 (* accounting for \n *)
      in
      seek l0 col0 (curline, txt, n, len) matches acc
  in
  let rec aux curline = function
    | [] -> []
    | (l, col, len)::matches when l <= curline + 1 ->
        let txt = input_line ch and curline = curline + 1 in
        assert (l = curline);
        let matches, curline, rets = seek curline col (curline, txt, col, len) matches [] in
        List.rev_append rets (aux curline matches)
    | matches ->
        while input_char ch <> '\n' do () done;
        aux (curline + 1) matches
  in
  seek_in ch 0;
  aux 0 matches

let print_lines color file lines =
  let rec collapse acc lines = match acc, lines with
    | (l0, txt, accl) :: accr,  (l, _, col, len) :: r when l0 = l ->
        collapse ((l0, txt, (col, len) :: accl) :: accr) r
    | acc, (l, txt, col, len) :: r ->
        collapse ((l, txt, [col, len]) :: acc) r
    | acc, [] -> List.rev acc
  in
  let shortfile = IndexMisc.make_relative file in
  let lines = collapse [] lines in
  let print_line =
    if not color then
      fun (l,txt, _) -> Printf.printf "%s:%d:%s\n" shortfile l txt
    else
      fun (l,txt,toks) ->
        Printf.printf "%s\027[36m:\027[m%d\027[36m:\027[m" shortfile l;
        let strlen = String.length txt in
        let ofs =
          List.fold_right (fun (col, len) ofs ->
              let ofs =
                if ofs < col then
                  (print_string (String.sub txt ofs (col - ofs));
                   print_string "\027[1;31m";
                   col)
                else ofs
              in
              let len = min (strlen - ofs) len in
              print_string (String.sub txt ofs len);
              print_string "\027[m";
              ofs + len)
            toks 0
        in
        if ofs < strlen then
          print_string (String.sub txt ofs (strlen - ofs));
        print_newline ()
  in
  List.iter print_line lines

let grep_file finder color file =
  try
    let ch = open_in file in
    let matches = List.rev (finder file ch) in
    (if matches <> [] then
       let lines = lines_of_file ch matches in
       print_lines color file lines);
    close_in ch;
    matches <> []
  with Sys_error _ as e ->
    Printf.eprintf "%s: %s\n%!" file (Printexc.to_string e); false

let grep pattern files color strings regexp =
  if strings || regexp then
    let re =
      Re.Posix.compile
        (if regexp then Re.Posix.re pattern else Re.str pattern)
    in
    List.fold_left
      (fun found f -> grep_file (Grep.strings_re re) color f || found)
      false files
  else
    let path = IndexMisc.(key_to_modpath (string_to_key pattern)) in
    List.fold_left
      (fun found f -> grep_file (Grep.ident path) color f || found)
      false files

let () =
  let open Cmdliner in
  let doc = "Locates instances of a given OCaml ident in source files, \
             handling (local) opens, module, etc." in
  let man = [
    `S "BUGS";
    `P "Current version doesn't handle shadowing and different kinds of idents, \
        therefore you can get false positive if a type and a value have the \
        name.";
    `P "Field records don't currently respect the distributive `{Module.' \
        syntax. Also, if you use record disambiguation, you're on your own \
        for field names since this program doesn't know about typing.";
  ]
  in
  match
    let cmd =
      Cmd.v
        (Cmd.info "ocp-grep" ~version:(Ocp_grep_version.version) ~doc ~man)
        Term.(const grep
               $ Args.pattern $ Args.files $ Args.color $ Args.strings $ Args.regexp)
    in
    Cmd.eval_value cmd       
  with
  | Ok (`Ok true) -> exit 0
  | Ok (`Ok false) -> exit 1
  | Error _ -> exit 2
  | _ -> exit 0

(* idea: single utility to color parts of source with syntactic context:
   pattern, expr, type, topexpr, module, record ...
   Could be used for better completion, analysis etc.*)
