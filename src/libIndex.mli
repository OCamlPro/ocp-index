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

(** Lazy trie structure holding the info on all identifiers *)
type t

(** Internal representation of types *)
type ty

(** The type of files we get our data from *)
type orig_file = private Cmt of string | Cmti of string | Cmi of string

(** Contains the information on a given identifier *)
type info = private {
  path: string list;
  kind: kind;
  name: string;
  ty: ty option;
  loc_sig: Location.t;
  loc_impl: Location.t Lazy.t;
  doc: string option Lazy.t;
  file: orig_file;
  (* library: string option *) }

(** The kind of elements that can be stored in the trie *)
and kind = private
  | Type | Value | Exception
  | Field of info | Variant of info
  | Method of info
  | Module | ModuleType
  | Class | ClassType


(** {2 Building} *)

(** Helper function, useful to lookup all subdirs of a given path before calling
    [load] *)
val unique_subdirs: string list -> string list

(** Build the trie from a list of include directories. They will be scanned for
    [.cmi] and [.cmt] files to complete on module names, and the contents of
    these files will be lazily read whenever needed. *)
val load: string list -> t

(** Load a single file into a trie *)
val add_file: t -> string -> t

(** Consider the module at the given path as opened, i.e. rebind its contents at
    the root of the trie. If [cleanup_path], also change its contents to refer
    to the new path. *)
val open_module: ?cleanup_path:bool -> t -> string list -> t

(** Same as [open_module], but tries to open even the elements that are not in
    the external interface (this needs a cmt to be present) *)
val fully_open_module: ?cleanup_path:bool -> t -> string list -> t

(** {2 Querying} *)

(** Returns all bindings in the trie *)
val all: t -> info list

(** Lookup an identifier in a trie (eg. [option] or [List.map]) *)
val get: t -> string -> info

(** Lookup identifiers starting with the given string. Completion stops at
    module boundaries (it wont unfold contents of modules) *)
val complete: t -> ?filter:(info -> bool) -> string -> info list


(** {2 Printing} *)

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
