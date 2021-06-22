(**************************************************************************)
(*                                                                        *)
(*  Copyright 2021 OCamlPro                                               *)
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

type sexp = A of string | T of sexp list

let atom str i =
  let rec escaped j =
    if j >= String.length str then j
    else match str.[j] with
      | '"' -> j+1
      | '\\' -> escaped (j+2)
      | _ -> escaped (j+1)
  in
  let rec unescaped j =
    if j >= String.length str then j
    else match str.[j] with
    | ' ' | '\n' | '\t' | '\r' | '(' | ')' -> j
    | '\\' -> unescaped (j+2)
    | _ -> unescaped (j+1)
  in
  let j = match str.[i] with
    | '"' -> escaped (i+1)
    | _ -> unescaped (i+1)
  in
  j, String.sub str i (j - i)

let rec sexp_parse str i =
  if i >= String.length str then i, []
  else
    match str.[i] with
    | ' ' | '\n' | '\t' | '\r' -> sexp_parse str (i+1)
    | '(' ->
        let i, t1 = sexp_parse str (i+1) in
        let i, t = sexp_parse str i in
        i, T t1 :: t
    | ')' -> i+1, []
    | ';' ->
        let i =
          try String.index_from str (i+1) '\n' + 1
          with Not_found -> String.length str
        in
        sexp_parse str i
    | _ ->
        let i, a = atom str i in
        let i, t = sexp_parse str i in
        i, A a :: t

let rec cut_list a acc = function
  | x :: r -> if x = a then acc, r else cut_list a (x::acc) r
  | [] -> acc, []

let module_eq name = function
  | A n -> IndexMisc.capitalize n = name
  | _ -> false

let list_find_opt f l = try Some (List.find f l) with Not_found -> None

let rec get_lib_name modname = function
  | T (A "library" :: t) :: r ->
      if List.mem (T [A "wrapped"; A "false"]) t then get_lib_name modname r
      else
        let libname =
          match
            list_find_opt (function T (A "name" :: _) -> true | _ -> false) t
          with
          | Some (T [_; A name]) -> name
          | _ ->
              match
                list_find_opt
                  (function T (A "public_name" :: _) -> true | _ -> false) t
              with
              | Some (T [_; A name]) ->
                  List.hd (List.rev (IndexMisc.string_split '.' name))
              | _ -> ""
        in
        (match
           list_find_opt (function T ( A "modules" :: _) -> true | _ -> false) t
         with
         | None -> Some libname
         | Some (T (_ :: ms)) ->
             let inc, exc = cut_list (A "\\") [] ms in
             if not (List.exists (module_eq modname) exc) &&
                List.exists (fun n -> module_eq modname n || n = A ":standard")
                  inc
             then Some libname
             else get_lib_name modname r
         | Some _ -> assert false)
  | _ :: r -> get_lib_name modname r
  | [] -> None

let string_of_channel ic =
  let n = 4096 in
  let s = Bytes.create n in
  let b = Buffer.create 1024 in
  let rec iter ic b s =
    let nread =
      try input ic s 0 n
      with End_of_file -> 0 in
    if nread > 0 then (
      Buffer.add_subbytes b s 0 nread;
      iter ic b s
    ) in
  iter ic b s;
  Buffer.contents b

let read_dune dir =
  try
    let ic = open_in (Filename.concat dir "dune") in
    let s = string_of_channel ic in
    let r = sexp_parse s 0 in
    close_in ic;
    Some r
  with Sys_error _ -> None

let rm_ext f = try Filename.chop_extension f with Invalid_argument _ -> f

let get_libname file =
  let modname =
    IndexMisc.capitalize (Filename.(basename (rm_ext file)))
  in
  match read_dune (Filename.dirname file) with
  | Some (_, sexp) -> get_lib_name modname sexp
  | None -> None
