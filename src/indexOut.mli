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


(** This module contains the output function for the [info] type bound to nodes
    in the trie, either through [Format] or directly to strings using [Print] *)

open IndexTypes

module Format: sig
  type coloriser =
    { f: 'a. kind ->
        ('a, Format.formatter, unit) format -> Format.formatter
        -> 'a }

  val color: coloriser
  val no_color: coloriser

  (** short name of the identifier *)
  val name: ?colorise:coloriser -> Format.formatter -> info -> unit

  (** fully qualified name *)
  val path: ?colorise:coloriser -> Format.formatter -> info -> unit

  val kind: ?colorise:coloriser -> Format.formatter -> info -> unit

  val ty: ?colorise:coloriser -> Format.formatter -> info -> unit

  val doc: ?colorise:coloriser -> Format.formatter -> info -> unit

  val loc:
    ?root:string -> ?intf:bool ->
    ?colorise:coloriser -> Format.formatter -> info -> unit

  val info: ?colorise:coloriser -> Format.formatter -> info -> unit
end

module Print: sig
  val name: ?color:bool -> info -> string
  val path: ?color:bool -> info -> string
  val kind: ?color:bool -> info -> string
  val ty: ?color:bool -> info -> string
  val doc: ?color:bool -> info -> string
  val loc: ?root:string -> ?intf:bool -> ?color:bool -> info -> string
  val info: ?color:bool -> info -> string
end
