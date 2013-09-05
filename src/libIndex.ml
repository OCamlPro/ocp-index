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

(* - Main types - *)

include IndexTypes

(* - Utility functions - *)

module Misc = IndexMisc

(* - Trie loading and manipulation functions - *)

include IndexBuild

(* - Query functions - *)

let filter_visible values =
  let same_kind a b = match a.kind, b.kind with
    | Field i, Field j | Variant i, Variant j | Method i, Method j ->
        i.name = j.name
    | a, b -> a = b
  in
  List.fold_left
    (fun acc info ->
       if List.exists (fun n -> same_kind info n) acc
       then acc else info::acc)
    [] values

let trie_to_list trie =
  Trie.fold0
    (fun acc _path values -> List.rev_append (filter_visible values) acc)
    trie []

let all t =
  trie_to_list t

let filter t f =
  Trie.fold0
    (fun acc _path values ->
       List.rev_append (filter_visible (List.filter f values)) acc)
    t []

let get t query = Trie.find t (Misc.string_to_list query)

let get_all t query = Trie.find_all t (Misc.string_to_list query)

let complete t ?filter:(f = fun _ -> true) query =
  filter
    (Trie.filter_keys ((<>) '.')
       (Trie.sub t (Misc.string_to_list query)))
    f

(* - Output functions - *)

include IndexOut
