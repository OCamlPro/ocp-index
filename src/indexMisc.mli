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

val debug_enabled: bool

val debug: ('a, out_channel, unit) format -> 'a

val timer: unit -> unit -> float

type key = char list
val dot: char
val dots: string

(** Used to get the keys (or paths) in the trie *)
val string_to_key: string -> key

val key_to_string: key -> string

val modpath_to_key: string list -> key

val key_to_modpath: key -> string list

val modpath_to_string: string list -> string

(** Returns the list of directories and all their recursive subdirectories *)
val unique_subdirs: string list -> string list

(** An heuristic to guess where the root directory of the current project.
    Returns (project_root, build_dir) *)
val project_root: ?path:string -> unit -> string option * string option

(** Locates a build dir within a given directory, based on name ([_build],
    [_obuild], etc.) *)
val find_build_dir: string -> string option
