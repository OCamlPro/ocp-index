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

val debug_enabled: bool

val debug: ('a, out_channel, unit) format -> 'a

val timer: unit -> unit -> float

(** Used to get the keys (or paths) in the trie *)
val string_to_list: string -> char list

val list_to_string: char list -> string

val modpath_to_list: string list -> char list

(** Returns the list of directories and all their recursive subdirectories *)
val unique_subdirs: string list -> string list
