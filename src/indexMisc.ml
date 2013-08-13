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


let debug_enabled =
  try match Sys.getenv "OCP_DEBUG" with "" | "0" -> false | _ -> true
  with Not_found -> false

let debug =
  if debug_enabled then
    fun fmt -> Printf.eprintf ("\027[36m"^^fmt^^"\027[m%!")
  else
    fun fmt -> Printf.ifprintf stderr fmt

let timer () =
  if debug_enabled then
    let t = Sys.time () in
    fun () -> Sys.time () -. t
  else
    fun () -> 0.

let string_to_list s =
  let rec aux acc i = if i >= 0 then aux (s.[i]::acc) (i - 1) else acc in
  aux [] (String.length s - 1)

let modpath_to_list path =
  List.fold_right (fun p acc -> string_to_list p @ '.' :: acc) path []

let list_to_string l =
  let rec aux n = function
    | [] -> String.create n
    | c::r -> let s = aux (n+1) r in s.[n] <- c; s
  in
  aux 0 l

let unique_subdirs dir_list =
  let rec subdirs acc path =
    Array.fold_left
      (fun acc p ->
        let path = Filename.concat path p in
        if try Sys.is_directory path with Sys_error _ -> false
        then subdirs acc path else acc)
      (path::acc)
      (Sys.readdir path)
  in
  let remove_dups l =
    let rec aux = function
      | a::(b::_ as r) when a = b -> aux r
      | a::r -> a :: aux r
      | [] -> []
    in
    aux (List.sort compare l)
  in
  remove_dups (List.fold_left subdirs [] dir_list)
