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
  let f = (Trie.filter_keys ((<>) dot) t2) in
  Trie.fold0
    (fun t path values ->
       let t =
         List.fold_left (fun t v -> Trie.add t path v)
           (Trie.unset t path) values
       in
       if List.exists (function
           | {kind=Module|ModuleType|Class|ClassType} -> true
           | _ -> false)
           values
       then
         let subpath = path @ [dot] in
         Trie.graft_lazy t subpath (lazy (Trie.sub t2 subpath))
       else t)
    f
    t1

let open_module ?(cleanup_path=false) t path =
  let strip_path = fix_path_prefix (List.length path) [] in
  let submodule = Trie.sub t (modpath_to_key path) in
  let submodule =
    if cleanup_path then Trie.map (fun _key -> strip_path) submodule
    else submodule
  in
  (* Trie.merge ~values:(fun _ v -> v) t submodule -- keeps hidden values in subtrees *)
  overriding_merge t submodule

let alias ?(cleanup_path=false) t origin alias =
  let subtree = Trie.sub t (modpath_to_key origin) in
  let subtree =
    let strip_path = fix_path_prefix (List.length origin) alias in
    if cleanup_path then Trie.map (fun _key -> strip_path) subtree
    else subtree
  in
  Trie.graft t (modpath_to_key alias) subtree

(* Pops comments from a list of comments (string * loc) to find the ones that
   are associated to a given location. Also returns the remaining comments after
   the location. *)
let associate_comment ?(after_only=false) comments loc =
  if loc = Location.none then None, comments else
  let lstart = loc.Location.loc_start.Lexing.pos_lnum
  and lend =  loc.Location.loc_end.Lexing.pos_lnum in
  let rec aux = function
    | [] -> None, []
    | (comment, cloc)::comments ->
        let cstart = cloc.Location.loc_start.Lexing.pos_lnum
        and cend =  cloc.Location.loc_end.Lexing.pos_lnum
        in
        if cend < lstart - 1 || cstart < lend && after_only then
          aux comments
        else if cstart > lend + 1 then
          None, (comment, cloc)::comments
        else if String.length comment < 2 ||
                comment.[0] <> '*' || comment.[1] = '*'
        then
          aux comments
        else
        let comment =
          String.trim (String.sub comment 1 (String.length comment - 1))
        in
        match aux comments with
        | None, comments -> Some comment, comments
        | Some c, comments -> Some (String.concat "\n" [comment; c]), comments
  in
  aux comments

let ty_of_sig_item =
  let open Printtyp in
  function
  | Types.Sig_value(id, decl) -> tree_of_value_description id decl
  | Types.Sig_type(id, decl, rs) -> tree_of_type_declaration id decl rs
  | Types.Sig_typext(id, decl, es) -> tree_of_extension_constructor id decl es
  | Types.Sig_module(id, { Types.md_type }, rs) -> tree_of_module id md_type rs
  | Types.Sig_modtype(id, decl) -> tree_of_modtype_declaration id decl
  | Types.Sig_class(id, decl, rs) -> tree_of_class_declaration id decl rs
  | Types.Sig_class_type(id, decl, rs) -> tree_of_cltype_declaration id decl rs

(* The types may contain unqualified identifiers.
   We need to do some (lazy) lookup in the trie to qualify them, so that
   for example [M.empty] shows up as [M.t] and not just [t] *)
let qualify_ty (parents:parents) ty =
  let qualify ident =
    let path =
      let rec get_path = function
        | Outcometree.Oide_ident name -> [name]
        | Outcometree.Oide_dot (path, s) -> get_path path @ [s]
        | Outcometree.Oide_apply (p1, _p2) -> get_path p1
      in
      get_path ident
    in
    let key = modpath_to_key path in
    let rec lookup = function
      | [] | ([],_) :: _ -> ident
      | ((path1::pathn), lazy t) :: parents ->
          if not (List.exists (fun id -> id.kind = Type) (Trie.find_all t key))
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
                      (fun acc modl -> Outcometree.Oide_dot (acc,modl))
                      (Outcometree.Oide_ident path1) pathn
                  in
                  Outcometree.Oide_dot (parentpath, s)
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
  | Osig_value (str, ty, str2) -> Osig_value (str, qual ty, str2)
  | Osig_typext (constr, es) ->
      Osig_typext ({ constr with
        oext_args = List.map qual constr.oext_args }, es)
  | out_sig -> out_sig (* don't get down in modules, classes and their types *)

let loc_of_sig_item = function
  | Types.Sig_value (_,descr) -> descr.Types.val_loc
  | Types.Sig_type (_,descr,_) -> descr.Types.type_loc
  | Types.Sig_typext (_,descr,_) -> descr.Types.ext_loc
  (* Sadly the Types tree doesn't contain locations for those. This means we
     won't associate comments easily either (todo...) *)
  | Types.Sig_module _
  | Types.Sig_modtype _
  | Types.Sig_class _
  | Types.Sig_class_type _
    -> Location.none

let id_of_sig_item = function
  | Types.Sig_value (id,_)
  | Types.Sig_type (id,_,_)
  | Types.Sig_typext (id,_,_)
  | Types.Sig_module (id,_,_)
  | Types.Sig_modtype (id,_)
  | Types.Sig_class (id,_,_)
  | Types.Sig_class_type (id,_,_)
    -> id

let kind_of_sig_item = function
  | Types.Sig_value _ -> Value
  | Types.Sig_type _ -> Type
  | Types.Sig_typext (_, _, Types.Text_exception) -> Exception
  | Types.Sig_typext _ -> OpenType
  | Types.Sig_module _ -> Module
  | Types.Sig_modtype _ -> ModuleType
  | Types.Sig_class _ -> Class
  | Types.Sig_class_type _ -> ClassType

let trie_of_type_decl ?comments info ty_decl =
  match ty_decl.Types.type_kind with
  | Types.Type_abstract -> [], comments
  | Types.Type_open -> [], comments
  | Types.Type_record (fields,_repr) ->
      List.map
        (fun { Types.ld_id; ld_type } ->
          let ty = Printtyp.tree_of_typexp false ld_type in
          let ty =
            Outcometree.Osig_type (Outcometree.{
                otype_name    = "";
                otype_params  = [];
                otype_type    = ty;
                otype_private = Asttypes.Public;
                otype_cstrs   = []; }, Outcometree.Orec_not)
          in
          string_to_key ld_id.Ident.name,
          Trie.create ~value:{
            path = info.path;
            orig_path = info.path;
            kind = Field info;
            name = ld_id.Ident.name;
            ty = Some ty;
            loc_sig = info.loc_sig;
            loc_impl = info.loc_impl;
            doc = lazy None;
            file = info.file;
          } ())
        fields,
      comments
  | Types.Type_variant variants ->
      List.map
        (fun { Types.cd_id; cd_args } ->
          let ty =
            let params = match cd_args with
              | [] -> Outcometree.Otyp_sum []
              | param::_ ->
                     Printtyp.tree_of_typexp false
                       { Types. desc = Types.Ttuple cd_args;
                         level = param.Types.level;
                         id = param.Types.id }
            in
            Outcometree.Osig_type (Outcometree.{
                otype_name    = "";
                otype_params  = [];
                otype_type    = params;
                otype_private = Asttypes.Public;
                otype_cstrs   = []; }, Outcometree.Orec_not)
          in
          string_to_key cd_id.Ident.name,
          Trie.create ~value:{
            path = info.path;
            orig_path = info.path;
            kind = Variant info;
            name = cd_id.Ident.name;
            ty = Some ty;
            loc_sig = info.loc_sig;
            loc_impl = info.loc_impl;
            doc = lazy None;
            file = info.file;
          } ())
        variants,
      comments

(* We usually load the info from the cmti file ; however, that doesn't give
   the implementation location. This function loads the cmt to get it. *)
let locate_impl cmt path name kind =
  try
    if not (Sys.file_exists cmt) then raise Not_found;
    let rec find_item path sign =
      match path with
      | [] ->
          List.find (fun item -> kind = kind_of_sig_item item &&
                                 name = (id_of_sig_item item).Ident.name)
            sign
      | modul::path ->
          let modul =
            List.find (fun item -> kind_of_sig_item item = Module &&
                                   modul = (id_of_sig_item item).Ident.name)
              sign
          in
          match modul with
          | Types.Sig_module (_, {Types.md_type = Types.Mty_signature sign},_) ->
              find_item path sign
          | _ -> raise Not_found
    in
    debug "Loading %s (looking up %s.%s)..." cmt (String.concat "." path) name;
    let chrono = timer () in
    let cmt_contents = Cmt_format.read_cmt cmt in
    debug " %.3fs ; now registering..." (chrono());
    let chrono = timer () in
    let sign =
      match cmt_contents.Cmt_format.cmt_annots with
      | Cmt_format.Implementation impl -> List.rev impl.Typedtree.str_type
      | _ -> raise Not_found
    in
    debug " %.3fs ; done\n%!" (chrono());
    let item = find_item path sign in
    loc_of_sig_item item
  with
  | Not_found -> Location.none

let rec trie_of_sig_item
    ?comments (parents:parents) (orig_file:orig_file) path sig_item
  =
  let id = id_of_sig_item sig_item in
  let loc = loc_of_sig_item sig_item in
  let doc, comments =
    match comments with
    | None -> lazy None, None
    | Some comments ->
        let assoc = lazy (associate_comment (Lazy.force comments) loc) in
        lazy (fst (Lazy.force assoc)),
        Some (lazy (snd (Lazy.force assoc)))
  in
  let ty = Some (ty_of_sig_item sig_item) in
  let kind = kind_of_sig_item sig_item in
  let loc_sig, loc_impl = match orig_file with
    | Cmi f | Cmti f ->
        loc, lazy (
          let cmt = Filename.chop_extension f ^ ".cmt" in
          locate_impl cmt (List.tl path) id.Ident.name kind
        )
    | Cmt _ ->
        (* we assume there is no mli, so point the intf to the implementation *)
        loc, Lazy.from_val loc
  in
  let info = {path; orig_path = path; kind; name = id.Ident.name; ty;
              loc_sig; loc_impl; doc; file = orig_file}
  in
  let siblings, comments = (* read fields / variants ... *)
    match sig_item with
    | Types.Sig_type (_id,descr,_is_rec) ->
        trie_of_type_decl ?comments info descr
    | _ -> [], comments
  in
  (* read module / class contents *)
  let children, comments =
    match sig_item with
    | Types.Sig_module (id,{ Types.md_type = Types.Mty_signature sign },_)
    | Types.Sig_modtype (id,{ Types.mtd_type = Some (Types.Mty_signature sign) })
      ->
        let path = path @ [id.Ident.name] in
        let rec children_comments = lazy (
          List.fold_left
            (fun (t,comments) sign ->
               let chlds,comments =
                 let siblings = lazy (fst (Lazy.force children_comments)) in
                 trie_of_sig_item ?comments ((path,siblings) :: parents)
                   orig_file path sign
               in
               List.fold_left Trie.append t chlds, comments)
            (Trie.empty,comments)
            sign
        ) in
        let children = lazy (fst (Lazy.force children_comments)) in
        let comments = match comments, children_comments with
          | None, _ -> None
          | Some _, lazy (_, comments) -> comments
        in
        children, comments
    | Types.Sig_module (_,{ Types.md_type = Types.Mty_ident sig_ident },_) ->
        let sig_path =
          let rec get_path = function
            | Path.Pident id -> [id.Ident.name]
            | Path.Pdot (path, s, _) -> get_path path @ [s]
            | Path.Papply (p1, _p2) -> get_path p1
          in
          get_path sig_ident
        in
        let sig_key = modpath_to_key sig_path in
        let rec lookup = function
          | [] -> Trie.empty
          | (parentpath, lazy t) :: parents ->
              let s = Trie.sub t sig_key in
              if s = Trie.empty then lookup parents else
                let rewrite_path =
                  fix_path_prefix
                    (List.length parentpath + List.length sig_path)
                    (path @ [id.Ident.name])
                in
                Trie.map (fun _k v -> rewrite_path v) s
        in
        lazy (lookup parents), comments
    | Types.Sig_class (id,{Types.cty_type=cty},_)
    | Types.Sig_class_type (id,{Types.clty_type=cty},_)
      ->
        let rec get_clsig = function
          | Types.Cty_constr (_,_,cty) | Types.Cty_arrow (_,_,cty) ->
              get_clsig cty
          | Types.Cty_signature clsig -> clsig
        in
        let clsig = get_clsig cty in
        let path = path@[id.Ident.name] in
        let (fields, _) =
          Ctype.flatten_fields (Ctype.object_fields clsig.Types.csig_self)
        in
        lazy (List.fold_left (fun t (lbl,_,ty_expr) ->
            if lbl = "*dummy method*" then t else
              let _ = Printtyp.reset_and_mark_loops ty_expr in
              let ty = Printtyp.tree_of_typexp false ty_expr in
              let ty =
                Outcometree.Osig_type (Outcometree.{
                    otype_name    = "";
                    otype_params  = [];
                    otype_type    = ty;
                    otype_private = Asttypes.Public;
                    otype_cstrs   = []; }, Outcometree.Orec_not)
              in
              Trie.add t (string_to_key lbl)
                { path = path;
                  orig_path = path;
                  kind = Method info;
                  name = lbl;
                  ty = Some ty;
                  loc_sig = loc_sig;
                  loc_impl = loc_impl;
                  doc = lazy None;
                  file = info.file })
          Trie.empty
          fields),
        comments
    | _ ->
        lazy Trie.empty, comments
  in
  let name = id.Ident.name in
  if String.length name > 0 && name.[0] = '#' then [], comments
  else
    (string_to_key id.Ident.name,
     Trie.create
       ~value:info
       ~children:(lazy [dot, Lazy.force children])
       ())
    :: siblings,
    comments

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
                Trie.sub (Lazy.force parent) (string_to_key (modl) @ [dot])
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
  Trie.map qualify t

let load_cmi root t modul orig_file =
  Trie.map_subtree t (string_to_key modul)
    (fun t ->
      let t =
        Trie.add t [] {
          path = [];
          orig_path = [];
          kind = Module;
          name = modul;
          ty = None;
          loc_sig = Location.none;
          loc_impl = Lazy.from_val Location.none;
          doc = lazy None;
          file = orig_file;
        }
      in
      let rec children = lazy (
        debug "Loading %s..." (orig_file_name orig_file);
        let chrono = timer () in
        let info = Cmi_format.read_cmi (orig_file_name orig_file) in
        debug " %.3fs ; now registering..." (chrono());
        let chrono = timer () in
        let t =
          List.fold_left
            (fun t sig_item ->
               let chld, _comments =
                 trie_of_sig_item [[modul], children; [], root]
                   orig_file [modul] sig_item
               in
               List.fold_left Trie.append t chld)
            Trie.empty
            info.Cmi_format.cmi_sign
        in
        debug " %.3fs ; done\n%!" (chrono());
        t
      )
      in
      let children = lazy (
        qualify_type_idents [[modul], children; [], root]
          (Lazy.force children)
      )
      in
      Trie.graft_lazy t [dot] children)

let load_cmt root t modul orig_file =
  Trie.map_subtree t (string_to_key modul)
    (fun t ->
      let t =
        Trie.add t [] {
          path = [];
          orig_path = [];
          kind = Module;
          name = modul;
          ty = None;
          loc_sig = Location.none;
          loc_impl = Lazy.from_val Location.none;
          doc = lazy None;
          file = orig_file;
        }
      in
      let rec children = lazy (
        debug "Loading %s..." (orig_file_name orig_file);
        let chrono = timer () in
        let info = Cmt_format.read_cmt (orig_file_name orig_file) in
        debug " %.3fs ; now registering..." (chrono());
        let chrono = timer () in
        let comments = Some (Lazy.from_val info.Cmt_format.cmt_comments) in
        let parents = [[modul], children; [], root] in
        let t =
          match info.Cmt_format.cmt_annots with
          | Cmt_format.Implementation {Typedtree.str_type = sign; _}
          | Cmt_format.Interface {Typedtree.sig_type = sign; _}
          | Cmt_format.Packed (sign, _)
            ->
              let t, _trailing_comments =
                List.fold_left
                  (fun (t,comments) sig_item ->
                     let chld, comments =
                       trie_of_sig_item ?comments parents orig_file
                         [modul] sig_item
                     in
                     List.fold_left Trie.append t chld, comments)
                  (Trie.empty, comments)
                  sign
              in
              t
          | _ -> Trie.empty
        in
        debug " %.3fs ; done\n%!" (chrono());
        t
      )
      in
      let children = lazy (
        qualify_type_idents [[modul], children; [], root]
          (Lazy.force children)
      )
      in
      Trie.graft_lazy t [dot] children)

let debug_file_counter = ref 0
let debug_dir_counter = ref 0

let load_file root t modul f =
  incr debug_file_counter;
  match f with
  | Cmi _ -> load_cmi root t modul f
  | Cmt _ | Cmti _ -> load_cmt root t modul f

let load_files t dir files =
  let split_filename file =
    try
      let i = String.rindex file '.' in
      let len = String.length file in
      let modul = String.capitalize (String.sub file 0 i) in
      let ext = String.lowercase (String.sub file (i+1) (len-i-1)) in
      modul, ext
    with Not_found -> file, ""
  in
  let sort_modules acc file =
    let reg base = Trie.add acc (string_to_key base) in
    match split_filename file with
    | base, "cmi" -> reg base (Cmi (Filename.concat dir file))
    | base, "cmt" -> reg base (Cmt (Filename.concat dir file))
    | base, "cmti" -> reg base (Cmti (Filename.concat dir file))
    | _ -> acc
  in
  let modules =
    List.fold_left sort_modules Trie.empty files
  in
  let rec root = lazy (
    Trie.fold0 (fun t modul files ->
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

let load_dir t dir =
  incr debug_dir_counter;
  let files = Array.to_list (Sys.readdir dir) in
  load_files t dir files

let load paths =
  let t = Trie.create () in
  let t =
    List.fold_left
      (fun t info ->
         Trie.add t (string_to_key info.name) info)
      t
      IndexPredefined.all
  in
  let chrono = timer () in
  let t = List.fold_left load_dir t paths in
  debug "Modules directory loaded in %.3fs (%d files in %d directories)...\n"
    (chrono()) !debug_file_counter !debug_dir_counter;
  open_module ~cleanup_path:true t ["Pervasives"]

let fully_open_module ?(cleanup_path=false) t path =
  let base_path = match path with
    | m::_ -> string_to_key m
    | [] -> []
  in
  (* Merge trying to keep the documentation if the new trie has none *)
  let merge intfs impls =
    let keep_intf info =
      try
        let intf = List.find (fun i -> i.kind = info.kind) intfs in
        let doc = lazy (match Lazy.force info.doc with
            | None -> Lazy.force intf.doc
            | some -> some)
        in
        let loc_sig = intf.loc_sig in
        { info with doc; loc_sig }
      with Not_found -> info
    in
    List.map keep_intf impls
  in
  let tpath = modpath_to_key path in
  let mod_trie = Trie.sub t tpath in
  let mod_trie =
    try match (Trie.find t base_path).file with
      | Cmti f | Cmi f ->
          let f = Filename.chop_extension f ^ ".cmt" in
          if not (Sys.file_exists f) then mod_trie
          else
            let dir,base = Filename.dirname f, Filename.basename f in
            let t = load_files Trie.empty dir [base] in
            let t = Trie.sub t tpath in
            Trie.merge ~values:merge mod_trie t
      | Cmt _ -> mod_trie
    with Not_found -> mod_trie
  in
  (* cleanup and merge at root (cf. open_module) *)
  let mod_trie =
    if cleanup_path then
      let pathlen = List.length path in
      Trie.map (fun _key -> fix_path_prefix pathlen []) mod_trie
    else mod_trie
  in
  overriding_merge t mod_trie

let add_file t file =
  let dir, file = Filename.dirname file, Filename.basename file in
  load_files t dir [file]
