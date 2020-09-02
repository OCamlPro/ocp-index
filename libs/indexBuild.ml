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


(* - Types - *)

open IndexTypes

(* This type is used to pass the full structure to functions building sub-trees,
   so that they can (lazily) lookup type or module type indirections in scope.
   Values of this type should have the form {[
     [ [Module;Submodule], lazy subtrie_at_Module.Submodule;
       [Module], lazy subtrie_at_Module;
       [], lazy subtrie_at_Root ]
   ]}
*)
type parents = (string list * t Lazy.t) list


(* - Utility functions - *)

open IndexMisc

let orig_file_name = function
  | Cmt f | Cmti f | Cmi f -> f

let equal_kind k1 k2 = match k1,k2 with
  | Type,Type | Value,Value | Exception,Exception
  | OpenType,OpenType
  | Field _,Field _ | Variant _,Variant _ | Method _,Method _
  | Module,Module | ModuleType,ModuleType
  | Class,Class | ClassType,ClassType
  | Keyword,Keyword ->
      true
  | Type,_ | Value,_ | Exception,_
  | OpenType,_
  | Field _,_ | Variant _,_ | Method _,_
  | Module,_ | ModuleType,_
  | Class,_ | ClassType,_
  | Keyword,_ ->
      false

let has_kind k info = equal_kind k info.kind

(* - Trie loading and manipulation functions - *)

(* Used as path separator *)
let dot = char_of_int 0

let fix_path_prefix strip new_pfx =
  let rec tln n = function
    | [] -> []
    | _::tl as l -> if n > 0 then tln (n-1) tl else l
  in
  let rev_pfx = List.rev new_pfx in
  fun id -> {id with path = List.rev_append rev_pfx (tln strip id.path)}

let overriding_merge t1 t2 =
  let f = (IndexTrie.filter_keys ((<>) dot) t2) in
  IndexTrie.fold0
    (fun t path values ->
       let t =
         List.fold_left (fun t v -> IndexTrie.add t path v)
           (IndexTrie.unset t path) values
       in
       if List.exists (function
           | {kind=Module|ModuleType|Class|ClassType} -> true
           | _ -> false)
           values
       then
         let subpath = path @ [dot] in
         IndexTrie.graft_lazy t subpath (lazy (IndexTrie.sub t2 subpath))
       else t)
    f
    t1

let open_module ?(cleanup_path=false) t path =
  let strip_path = fix_path_prefix (List.length path) [] in
  let submodule = IndexTrie.sub t (modpath_to_key path) in
  let submodule =
    if cleanup_path then IndexTrie.map (fun _key -> strip_path) submodule
    else submodule
  in
  (* IndexTrie.merge ~values:(fun _ v -> v) t submodule -- keeps hidden values in subtrees *)
  overriding_merge t submodule

let alias ?(cleanup_path=false) t origin alias =
  let subtree = IndexTrie.sub t (modpath_to_key origin) in
  let subtree =
    let strip_path = fix_path_prefix (List.length origin) alias in
    if cleanup_path then IndexTrie.map (fun _key -> strip_path) subtree
    else subtree
  in
  IndexTrie.graft t (modpath_to_key alias) subtree

(* -- Qualifying types -- *)

#if OCAML_VERSION >= (4,08,0)
  let n s = {Outcometree.printed_name = s}
  let nn {Outcometree.printed_name} = printed_name
#else
  let n s = s
  let nn s = s
#endif

(* The types may contain unqualified identifiers.
   We need to do some (lazy) lookup in the trie to qualify them, so that
   for example [M.empty] shows up as [M.t] and not just [t] *)
let qualify_ty (parents:parents) ty =
  let qualify ident =
    let path =
      let rec get_path = function
        | Outcometree.Oide_ident name -> [nn name]
        | Outcometree.Oide_dot (path, s) -> get_path path @ [s]
        | Outcometree.Oide_apply (p1, _p2) -> get_path p1
      in
      get_path ident
    in
    let key = modpath_to_key ~enddot:false path in
    let rec lookup = function
      | [] | ([],_) :: _ -> ident
      | ((path1::pathn), lazy t) :: parents ->
          if not (List.exists (has_kind Type) (IndexTrie.find_all t key))
          then lookup parents
          else
            let rec add_pfx = function
              | Outcometree.Oide_dot (idp, s) ->
                  Outcometree.Oide_dot (add_pfx idp, s)
              | Outcometree.Oide_apply (idp, idp2) ->
                  Outcometree.Oide_apply (add_pfx idp, idp2)
              | Outcometree.Oide_ident s ->
                  let parentpath =
                    List.fold_left
                      (fun acc modl -> Outcometree.Oide_dot (acc, modl))
                      (Outcometree.Oide_ident (n path1)) pathn
                  in
                  Outcometree.Oide_dot (parentpath, nn s)
            in add_pfx ident
    in
    lookup parents
  in
  let rec aux = (* Some kind of Outcometree.map_ty *)
    let open Outcometree in
    function
    | Otyp_abstract -> Otyp_abstract
    | Otyp_alias (ty, str) -> Otyp_alias (aux ty, str)
    | Otyp_arrow (str, ty1, ty2) -> Otyp_arrow (str, aux ty1, aux ty2)
    | Otyp_class (bl, id, tylist) ->
        Otyp_class (bl, qualify id, List.map aux tylist)
    | Otyp_constr (id, tylist) ->
        Otyp_constr (qualify id, List.map aux tylist)
    | Otyp_manifest (ty1, ty2) -> Otyp_manifest (aux ty1, aux ty2)
    | Otyp_object (strtylist, blopt) ->
        Otyp_object (List.map (fun (str,ty) -> str, aux ty) strtylist, blopt)
    | Otyp_record (strbltylist) ->
        Otyp_record (List.map (fun (str,bl,ty) -> str, bl, aux ty) strbltylist)
    | Otyp_stuff str -> Otyp_stuff str
    | Otyp_sum (strtylisttyoptlist) ->
        Otyp_sum
          (List.map (fun (str,tylist,tyopt) ->
               str, List.map aux tylist,
               match tyopt with Some ty -> Some (aux ty)
                              | None -> None)
              strtylisttyoptlist)
    | Otyp_tuple (tylist) -> Otyp_tuple (List.map aux tylist)
    | Otyp_var (bl, str) -> Otyp_var (bl, str)
    | Otyp_variant (bl, var, bl2, strlistopt) ->
        Otyp_variant (bl, var, bl2, strlistopt)
    | Otyp_poly (str, ty) -> Otyp_poly (str, aux ty)
    | Otyp_module (str, strl, tylist) ->
        Otyp_module (str, strl, List.map aux tylist)
    | Otyp_open -> Otyp_open
#if OCAML_VERSION >= (4,03,0)
    | Otyp_attribute (ty,attr) -> Otyp_attribute (aux ty, attr)
#endif
  in
  aux ty

let qualify_ty_in_sig_item (parents:parents) =
  let qual = qualify_ty parents in
  let open Outcometree in
  function

  | Osig_type (out_type_decl, rc) ->
      Osig_type ({ out_type_decl with
        otype_type  = qual out_type_decl.otype_type;
        otype_cstrs = List.map (fun (ty1,ty2) -> qual ty1, qual ty2)
            out_type_decl.otype_cstrs }, rc)

#if OCAML_VERSION >= (4,03,0)
  | Osig_value o -> Osig_value {o with oval_type = qual o.oval_type}
#else
  | Osig_value (str, ty, str2) -> Osig_value (str, qual ty, str2)
#endif

  | Osig_typext (constr, es) ->
      Osig_typext ({ constr with
                     oext_args = List.map qual constr.oext_args }, es)

  | out_sig -> out_sig (* don't get down in modules, classes and their types *)

(* -- end -- *)

let add_locs ~locs t =
  IndexTrie.map (fun path info ->
      let loc_info = lazy (
        List.find (has_kind info.kind) (IndexTrie.find_all locs path)
      ) in
      let lookup fld none =
        let loc = Lazy.force (fld info) in
        if loc = none
        then try Lazy.force (fld (Lazy.force loc_info)) with Not_found -> none
        else loc
      in
      { info with
        loc_sig = lazy (lookup (fun i -> i.loc_sig) Location.none);
        loc_impl = lazy (lookup (fun i -> i.loc_impl) Location.none);
        doc = lazy (lookup (fun i -> i.doc) None);
      }
    ) t

(* Can work in a subtree (t doesn't have to be the root) *)
let qualify_type_idents parents t =
  let qualify _key id =
    let rel_path =
      let rec rm_pfx parents path = match parents,path with
        | [_root], path -> path
        | _::parents, _::path -> rm_pfx parents path
        | _ -> assert false
      in
      rm_pfx parents id.path
    in
    let qualify_ty ty =
      let parents =
        let rec aux acc path = match acc,path with
          | ((pfx, parent) :: _), modl::r ->
              let t = lazy (
                IndexTrie.sub (Lazy.force parent) (string_to_key (modl) @ [dot])
              ) in
              aux ((pfx @ [modl], t) :: acc) r
          | _ -> acc
        in
        aux parents rel_path
      in
      qualify_ty_in_sig_item parents ty
    in
    { id with ty = match id.ty with Some ty -> Some (qualify_ty ty)
                                  | None -> None }
  in
  IndexTrie.map qualify t

(* Look for a cmt file for the purpose of loading implementation locations.
   (assuming other information is already loaded eg. from the cmti). *)
let lookup_loc_impl orig_file =
  match orig_file with
  | Cmt _ -> None
  | Cmi f | Cmti f ->
      let cmt = Filename.chop_extension f ^ ".cmt" in
      if Sys.file_exists cmt then Some cmt
      else
        let dir = Filename.dirname cmt in
        (* dune 2 puts .cmt under native/ while cmi and cmti are under byte/ *)
        if Filename.basename dir = "byte" then
          let ( / ) = Filename.concat in
          let cmt = Filename.dirname dir / "native" / Filename.basename cmt in
          if Sys.file_exists cmt then Some cmt
          else None
        else None

module Ocaml_cur = struct
  module Ident = Ident
  module Path = Path
  module Outcometree = Outcometree
  module Lexing = Lexing
  module Location = Location
end

module type ARTIFACT_READER = sig
  val load_cmt:
    ?qualify:bool ->
    t Lazy.t ->
    (char, info) IndexTrie.t ->
    string -> orig_file -> (char, info) IndexTrie.t

  val load_cmi:
    ?qualify:bool ->
    t Lazy.t ->
    (char, info) IndexTrie.t ->
    string -> orig_file -> (char, info) IndexTrie.t

  val cmi_magic_number: string
  val cmt_magic_number: string
end

module V408: ARTIFACT_READER = struct
  module Ocaml_v = Ocaml_408
  let ocaml_v = Migrate_parsetree.Versions.ocaml_408
  #define OCAMLV (4,08,0)
  #include "indexArtifactReader.mlp"
  #undef OCAMLV
end

let debug_file_counter = ref 0
let debug_dir_counter = ref 0

let (readers: (module ARTIFACT_READER) list)  = [
  (module V408);
]

let get_reader =
  let map =
    List.fold_left
      (fun acc (module O: ARTIFACT_READER) ->
         (O.cmt_magic_number, O.load_cmt) ::
         (O.cmi_magic_number, O.load_cmi) ::
         acc)
      []
      readers
  in
  fun magic -> List.assoc magic map

let load_file root t modul f =
  incr debug_file_counter;
  let get_magic fn =
    let ic = open_in_bin fn in
    try
      let m = really_input_string ic (String.length Config.cmt_magic_number)in
      close_in ic; m
    with e -> close_in ic; raise e
  in
  let fn = match f with Cmi fn | Cmt fn | Cmti fn -> fn in
  let reader =
    try get_reader (get_magic fn)
    with Not_found -> raise (Bad_format fn)
  in
  reader root t modul f

let load_files t dirfiles =
  let split_filename file =
    try
      let i = String.rindex file '.' in
      let len = String.length file in
#if OCAML_VERSION >= (4,03,0)
      let modul = String.capitalize_ascii (String.sub file 0 i) in
      let ext = String.lowercase_ascii (String.sub file (i+1) (len-i-1)) in
#else
      let modul = String.capitalize (String.sub file 0 i) in
      let ext = String.lowercase (String.sub file (i+1) (len-i-1)) in
#endif
      modul, ext
    with Not_found -> file, ""
  in
  let sort_modules acc (dir,file) =
    let reg base = IndexTrie.add acc (string_to_key base) in
    match split_filename file with
    | base, "cmi" -> reg base (Cmi (Filename.concat dir file))
    | base, "cmt" -> reg base (Cmt (Filename.concat dir file))
    | base, "cmti" -> reg base (Cmti (Filename.concat dir file))
    | _ -> acc
  in
  let modules =
    List.fold_left sort_modules IndexTrie.empty dirfiles
  in
  let rec root = lazy (
    IndexTrie.fold0 (fun t modul files ->
        match files with
        | [] -> t
        | f1::fs ->
            (* Load by order of priority:
               - first cmti, more info than cmi, ocamldocs, and doesn't expose
                 private values
               - then cmt, it means there is no mli, everything is exposed
               - then cmi, has the interface if not much more *)
            let choose_file f1 f2 = match f1,f2 with
              | (Cmti _ as f), _ | _, (Cmti _ as f)
              | (Cmt _ as f), _ | _, (Cmt _ as f)
              | (Cmi _ as f), _ -> f
            in
            let file = List.fold_left choose_file f1 fs in
            let modul = key_to_string modul in
            load_file root t modul file)
      modules
      t
  )
  in Lazy.force root

let load_dirs t dirs =
  let dirfiles =
    List.fold_left (fun acc dir ->
        incr debug_dir_counter;
        let files =
          List.rev_map (fun f -> dir, f) (Array.to_list (Sys.readdir dir))
        in
        List.rev_append files acc)
      []
      (List.rev dirs)
  in
  load_files t dirfiles

let load paths =
  let t = IndexTrie.create () in
  let t =
    List.fold_left
      (fun t info ->
         IndexTrie.add t (string_to_key info.name) info)
      t
      IndexPredefined.all
  in
  let chrono = timer () in
  let t = load_dirs t paths in
  debug "Modules directory loaded in %.3fs (%d files in %d directories)...\n"
    (chrono()) !debug_file_counter !debug_dir_counter;
#if OCAML_VERSION >= (4,07,0)
  open_module ~cleanup_path:true t ["Stdlib"]
#else
  open_module ~cleanup_path:true t ["Pervasives"]
#endif

let fully_open_module ?(cleanup_path=false) t path =
  let base_path = match path with
    | m::_ -> string_to_key m
    | [] -> []
  in
  (* Merge trying to keep the documentation if the new trie has none *)
  let merge intfs impls =
    let keep_intf info =
      try
        let intf = List.find (has_kind info.kind) intfs in
        let doc = lazy (match info.doc with
            | lazy None -> Lazy.force intf.doc
            | lazy some -> some)
        in
        let loc_sig = intf.loc_sig in
        { info with doc; loc_sig }
      with Not_found -> info
    in
    List.map keep_intf impls
  in
  let tpath = modpath_to_key path in
  let mod_trie = IndexTrie.sub t tpath in
  let mod_trie =
    try match (IndexTrie.find t base_path).file with
      | Cmti f | Cmi f ->
          let f = Filename.chop_extension f ^ ".cmt" in
          if not (Sys.file_exists f) then mod_trie
          else
            let dir,base = Filename.dirname f, Filename.basename f in
            let t = load_files IndexTrie.empty [dir,base] in
            let t = IndexTrie.sub t tpath in
            IndexTrie.merge ~values:merge mod_trie t
      | Cmt _ -> mod_trie
    with Not_found -> mod_trie
  in
  (* cleanup and merge at root (cf. open_module) *)
  let mod_trie =
    if cleanup_path then
      let pathlen = List.length path in
      IndexTrie.map (fun _key -> fix_path_prefix pathlen []) mod_trie
    else mod_trie
  in
  overriding_merge t mod_trie

let add_file t file =
  let dir, file = Filename.dirname file, Filename.basename file in
  load_files t [dir,file]
