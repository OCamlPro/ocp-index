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


(* - Main types - *)

(* How we keep types represented internally *)
type ty = Outcometree.out_sig_item

type orig_file = Cmt of string | Cmti of string | Cmi of string

type info = { path: string list;
              kind: kind;
              name: string;
              ty: ty option;
              loc_sig: Location.t;
              loc_impl: Location.t Lazy.t;
              doc: string option Lazy.t;
              file: orig_file;
           (* library: string option *) }

and kind =
  | Type | Value | Exception
  | Field of info | Variant of info
  | Method of info
  | Module | ModuleType
  | Class | ClassType

type t = (char, info) Trie.t

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

let option_iter opt f = match opt with
  | Some x -> f x
  | None -> ()

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

let orig_file_name = function
  | Cmt f | Cmti f | Cmi f -> f

(* - Trie loading and manipulation functions - *)

let fix_path_prefix strip new_pfx =
  let rec tln n = function
    | [] -> []
    | _::tl as l -> if n > 0 then tln (n-1) tl else l
  in
  let rev_pfx = List.rev new_pfx in
  fun id -> {id with path = List.rev_append rev_pfx (tln strip id.path)}

let open_module ?(cleanup_path=false) t path =
  let strip_path = fix_path_prefix (List.length path) [] in
  let submodule = Trie.sub t (modpath_to_list path) in
  let submodule =
    if cleanup_path then Trie.map (fun _key -> strip_path) submodule
    else submodule
  in
  Trie.merge t submodule

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
  | Types.Sig_exception(id, decl) -> tree_of_exception_declaration id decl
  | Types.Sig_module(id, mty, rs) -> tree_of_module id mty rs
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
    let key = string_to_list (String.concat "." path) in
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
  in
  aux ty

let qualify_ty_in_sig_item (parents:parents) =
  let qual = qualify_ty parents in
  let open Outcometree in
  function
  | Osig_type ((str, list, ty, priv, tylist2), rc) ->
      Osig_type ((str, list, qual ty, priv,
        List.map (fun (ty1,ty2) -> qual ty1, qual ty2) tylist2), rc)
  | Osig_value (str, ty, str2) -> Osig_value (str, qual ty, str2)
  | Osig_exception (str, tylist) -> Osig_exception (str, List.map qual tylist)
  | out_sig -> out_sig (* don't get down in modules, classes and their types *)

let loc_of_sig_item = function
  | Types.Sig_value (_,descr) -> descr.Types.val_loc
  | Types.Sig_type (_,descr,_) -> descr.Types.type_loc
  | Types.Sig_exception (_,descr) -> descr.Types.exn_loc
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
  | Types.Sig_exception (id,_)
  | Types.Sig_module (id,_,_)
  | Types.Sig_modtype (id,_)
  | Types.Sig_class (id,_,_)
  | Types.Sig_class_type (id,_,_)
    -> id

let kind_of_sig_item = function
  | Types.Sig_value _ -> Value
  | Types.Sig_type _ -> Type
  | Types.Sig_exception _ -> Exception
  | Types.Sig_module _ -> Module
  | Types.Sig_modtype _ -> ModuleType
  | Types.Sig_class _ -> Class
  | Types.Sig_class_type _ -> ClassType

let trie_of_type_decl ?comments info ty_decl =
  match ty_decl.Types.type_kind with
  | Types.Type_abstract -> [], comments
  | Types.Type_record (fields,_repr) ->
      List.map
        (fun (id, _mutable, ty_expr) ->
          let ty = Printtyp.tree_of_typexp false ty_expr in
          let ty =
            Outcometree.Osig_type
              (("", [], ty, Asttypes.Public, []), Outcometree.Orec_not)
          in
          string_to_list id.Ident.name,
          Trie.create ~value:{
            path = info.path;
            kind = Field info;
            name = id.Ident.name;
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
        (fun (id, ty_exprs, _constraints) ->
          let ty =
            let params = match ty_exprs with
              | [] -> Outcometree.Otyp_sum []
              | param::_ ->
                     Printtyp.tree_of_typexp false
                       { Types. desc = Types.Ttuple ty_exprs;
                         level = param.Types.level;
                         id = param.Types.id }
            in
            Outcometree.Osig_type
              (("", [], params, Asttypes.Public, []), Outcometree.Orec_not)
          in
          string_to_list id.Ident.name,
          Trie.create ~value:{
            path = info.path;
            kind = Variant info;
            name = id.Ident.name;
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
          | Types.Sig_module (_,Types.Mty_signature sign,_) ->
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
  let info = {path; kind; name = id.Ident.name; ty;
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
    | Types.Sig_module (id,Types.Mty_signature sign,_)
    | Types.Sig_modtype (id,Types.Modtype_manifest (Types.Mty_signature sign))
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
    | Types.Sig_module (_,Types.Mty_ident sig_ident,_) ->
        let sig_path =
          let rec get_path = function
            | Path.Pident id -> [id.Ident.name]
            | Path.Pdot (path, s, _) -> get_path path @ [s]
            | Path.Papply (p1, _p2) -> get_path p1
          in
          get_path sig_ident
        in
        let sig_key = modpath_to_list sig_path in
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
          | Types.Cty_constr (_,_,cty) | Types.Cty_fun (_,_,cty) ->
              get_clsig cty
          | Types.Cty_signature clsig -> clsig
        in
        let clsig = get_clsig cty in
        let path = path@[id.Ident.name] in
        let (fields, _) =
          Ctype.flatten_fields (Ctype.object_fields clsig.Types.cty_self)
        in
        lazy (List.fold_left (fun t (lbl,_,ty_expr) ->
            if lbl = "*dummy method*" then t else
              let _ = Printtyp.reset_and_mark_loops ty_expr in
              let ty = Printtyp.tree_of_typexp false ty_expr in
              let ty =
                Outcometree.Osig_type
                  (("", [], ty, Asttypes.Public, []), Outcometree.Orec_not)
              in
              Trie.add t (string_to_list lbl)
                { path = path;
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
    (string_to_list id.Ident.name,
     Trie.create
       ~value:info
       ~children:(lazy ['.', Lazy.force children])
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
                Trie.sub (Lazy.force parent) (string_to_list (modl ^ "."))
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
  Trie.map_subtree t (string_to_list modul)
    (fun t ->
      let t =
        Trie.add t [] {
          path = [];
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
      Trie.graft_lazy t ['.'] children)

let load_cmt root t modul orig_file =
  Trie.map_subtree t (string_to_list modul)
    (fun t ->
      let t =
        Trie.add t [] {
          path = [];
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
          | _ ->
              Printf.eprintf
                "\027[33mWarning: %S: unhandled cmt format.\027[m\n%!"
                (orig_file_name orig_file);
              t
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
      Trie.graft_lazy t ['.'] children)

(* Predifined elements which are not in Pervasives
   List and doc taken from
   http://caml.inria.fr/pub/docs/manual-ocaml/manual034.html (4.00.1) *)
let predefined : info list =
  let open Outcometree in
  let mktype name ?(params=[]) ?(def=Otyp_abstract) doc = {
    path = [];
    kind = Type;
    name = name;
    ty = Some (Osig_type (
        (name,List.map (fun v -> v,(true,true)) params,def,Asttypes.Public,[]),
        Orec_not));
    loc_sig = Location.none;
    loc_impl = lazy Location.none;
    doc = lazy (Some doc);
    file = Cmi "*built-in*";
  } in
  let mkvariant name parent params = {
    path = [];
    kind = Variant parent;
    name = name;
    ty = Some (Osig_type (("", [],
                           (match params with [] -> Otyp_sum []
                                            | l -> Otyp_tuple l),
                           Asttypes.Public, []),
                          Outcometree.Orec_not));
    loc_sig = Location.none;
    loc_impl = lazy Location.none;
    doc = lazy None;
    file = Cmi "*built-in*";
  } in
  let mkexn name params doc = {
    path = [];
    kind = Exception;
    name = name;
    ty = Some (Osig_exception (name,params));
    loc_sig = Location.none;
    loc_impl = lazy Location.none;
    doc = lazy (Some doc);
    file = Cmi "*built-in*";
  }
  in
  let var name = Otyp_var (false, name) in
  let constr ?(params=[]) name =
    Otyp_constr (Oide_ident name, List.map var params)
  in
  let ibool =
    mktype "bool" ~def:(Otyp_sum ["true",[],None; "false",[],None])
      "The type of booleans (truth values)."
  in let itrue = mkvariant "true" ibool []
  in let ifalse = mkvariant "false" ibool []
  in
  let ilist =
    mktype "list"
      ~params:["'a"]
      ~def:(Otyp_sum ["[]", [], None;
                      "::", [var "a"; constr ~params:["a"] "list"], None])
      "The type of lists whose elements have type 'a."
  in let icons = mkvariant "::" ilist [var "a"; constr ~params:["a"] "list"]
  in let inil = mkvariant "[]" ilist []
  in
  let ioption =
    mktype "option"
      ~def:(Otyp_sum ["None",[],None; "Some", [var "a"], None])
      "The type of optional values of type 'a."
  in let isome = mkvariant "Some" ioption [var "a"]
  in let inone = mkvariant "None" ioption []
  in [
    mktype "int" "The type of integer numbers.";
    mktype "char" "The type of characters.";
    mktype "string" "The type of character strings.";
    mktype "float" "The type of floating-point numbers.";
    ibool; itrue; ifalse;
    mktype "unit" ~def:(Otyp_stuff "()") "The type of the unit value.";
    mktype "exn" "The type of exception values.";
    mktype "array" "The type of arrays whose elements have type 'a.";
    ilist; inil; icons;
    ioption; isome; inone;
    mktype "int32"
      "The type of signed 32-bit integers. See the Int32 module.";
    mktype "int64"
      "The type of signed 64-bit integers. See the Int64 module.";
    mktype "nativeint"
      "The type of signed, platform-native integers (32 bits on 32-bit \
       processors, 64 bits on 64-bit processors). See the Nativeint module.";
    mktype "format6"
      "The type of format strings. 'a is the type of the parameters of the \
       format, 'f is the result type for the printf-style functions, 'b is the \
       type of the first argument given to %a and %t printing functions (see \
       module Printf), 'c is the result type of these functions, and also the \
       type of the argument transmitted to the first argument of kprintf-style \
       functions, 'd is the result type for the scanf-style functions (see \
       module Scanf), and 'e is the type of the receiver function for the \
       scanf-style functions.";
    mktype "lazy_t"
      "This type is used to implement the Lazy module. It should not be used \
       directly.";
    mkexn "Match_failure" [constr "string"; constr "int"; constr "int"]
      "Exception raised when none of the cases of a pattern-matching apply. \
       The arguments are the location of the match keyword in the source code \
       (file name, line number, column number).";
    mkexn "Assert_failure" [constr "string"; constr "int"; constr "int"]
      "Exception raised when an assertion fails. The arguments are the \
       location of the assert keyword in the source code (file name, line \
       number, column number).";
    mkexn "Invalid_argument" [constr "string"]
      "Exception raised by library functions to signal that the given \
       arguments do not make sense.";
    mkexn "Failure" [constr "string"]
      "Exception raised by library functions to signal that they are undefined \
       on the given arguments.";
    mkexn "Not_found" []
      "Exception raised by search functions when the desired object could not \
       be found.";
    mkexn "Out_of_memory" []
      "Exception raised by the garbage collector when there is insufficient \
       memory to complete the computation.";
    mkexn "Stack_overflow" []
      "Exception raised by the bytecode interpreter when the evaluation stack \
       reaches its maximal size. This often indicates infinite or excessively \
       deep recursion in the user’s program.";
    mkexn "Sys_error" [constr "string"]
      "Exception raised by the input/output functions to report an operating \
       system error.";
    mkexn "End_of_file" []
      "Exception raised by input functions to signal that the end of file has \
       been reached.";
    mkexn "Division_by_zero" []
      "Exception raised by integer division and remainder operations when \
       their second argument is zero.";
    mkexn "Sys_blocked_io" []
      "A special case of Sys_error raised when no I/O is possible on a \
       non-blocking I/O channel.";
    mkexn "Undefined_recursive_module"
      [constr "string"; constr "int"; constr "int"]
      "Exception raised when an ill-founded recursive module definition is \
       evaluated. The arguments are the location of the definition in the \
       source code (file name, line number, column number).";
  ]

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
    let reg base = Trie.add acc (string_to_list base) in
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
            let modul = list_to_string modul in
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
         Trie.add t (string_to_list info.name) info)
      t
      predefined
  in
  let chrono = timer () in
  let t = List.fold_left load_dir t paths in
  debug "Modules directory loaded in %.3fs (%d files in %d directories)...\n"
    (chrono()) !debug_file_counter !debug_dir_counter;
  open_module ~cleanup_path:true t ["Pervasives"]

let fully_open_module ?(cleanup_path=false) t path =
  let base_path = match path with
    | m::_ -> string_to_list m
    | [] -> []
  in
  (* Merge trying to keep the documentation if the new trie has none *)
  let merge v1 v2 =
    let fallback_doc info = match Lazy.force info.doc with
      | Some _ as some -> some
      | None ->
          try Lazy.force (List.find (fun i -> i.kind = info.kind) v1).doc
          with Not_found -> None
    in
    List.map (fun info -> { info with doc = lazy (fallback_doc info) }) v2
  in
  let tpath = modpath_to_list path in
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
  Trie.merge t mod_trie

let add_file t file =
  let dir, file = Filename.dirname file, Filename.basename file in
  load_files t dir [file]

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

let get t query = Trie.find t (string_to_list query)

let complete t ?filter:(f = fun _ -> true) query =
  filter
    (Trie.filter_keys ((<>) '.')
       (Trie.sub t (string_to_list query)))
    f


(* - Output - *)

module IndexFormat = struct

  let list
      ?(paren=false) ?(left=fun _ -> ()) ?(right=fun _ -> ())
      pr sep fmt lst
    =
    let rec aux = function
      | [] -> ()
      | [x] -> pr fmt x
      | x::r -> pr fmt x; sep fmt (); aux r
    in
    match lst with
    | [] -> ()
    | [x] -> left fmt; pr fmt x; right fmt
    | _::_::_ ->
        if paren then Format.pp_print_char fmt '(';
        left fmt; aux lst; right fmt;
        if paren then Format.pp_print_char fmt ')'

  let lines fmt str =
    let len = String.length str in
    let rec aux i =
      if i >= len then () else
        let j = try String.index_from str i '\n' with Not_found -> len in
        Format.pp_print_string fmt (String.trim (String.sub str i (j - i)));
        if j < len - 1 then
          (Format.pp_force_newline fmt ();
           aux (j+1))
    in
    aux 0

  type coloriser =
    { f: 'a. kind ->
        ('a, Format.formatter, unit) format -> Format.formatter
        -> 'a }

  let color =
    let f kind fstr fmt =
      let colorcode = match kind with
        | Type -> "\027[36m"
        | Value -> "\027[1m"
        | Exception -> "\027[33m"
        | Field _ | Variant _ -> "\027[34m"
        | Method _ -> "\027[1m"
        | Module | ModuleType -> "\027[31m"
        | Class | ClassType -> "\027[35m"
      in
      Format.pp_print_as fmt 0 colorcode;
      Format.kfprintf (fun fmt -> Format.pp_print_as fmt 0 "\027[m") fmt fstr
    in { f }

  let no_color =
    let f _ fstr fmt = Format.fprintf fmt fstr in
    { f }

  let name ?(colorise = no_color) fmt id =
    colorise.f id.kind "%s" fmt id.name

  let path ?(colorise = no_color) fmt id =
    List.iter
      (Format.fprintf fmt "%a." (colorise.f Module "%s"))
      id.path;
    name ~colorise fmt id

  let kind ?(colorise = no_color) fmt id =
    match id.kind with
    | Type -> Format.pp_print_string fmt "type"
    | Value -> Format.pp_print_string fmt "val"
    | Exception -> Format.pp_print_string fmt "exception"
    | Field parentty ->
        Format.fprintf fmt "field(%a)"
          (colorise.f parentty.kind "%s") parentty.name
    | Variant parentty ->
        Format.fprintf fmt "constr(%a)"
          (colorise.f parentty.kind "%s") parentty.name
    | Method parentclass ->
        Format.fprintf fmt "method(%a)"
          (colorise.f parentclass.kind "%s") parentclass.name
    | Module -> Format.pp_print_string fmt "module"
    | ModuleType -> Format.pp_print_string fmt "modtype"
    | Class -> Format.pp_print_string fmt "class"
    | ClassType -> Format.pp_print_string fmt "classtype"

  let rec tydecl fmt =
    let open Outcometree in
    function
    | Otyp_abstract -> ()
    | Otyp_manifest (ty,_) -> tydecl fmt ty
    | Otyp_record fields ->
        let print_field fmt (name, mut, arg) =
          Format.fprintf fmt "@[<2>%s%s :@ %a@];"
            (if mut then "mutable " else "") name
            !Oprint.out_type arg
        in
        Format.fprintf fmt "{%a@;<1 -2>}"
          (list ~left:(fun fmt -> Format.pp_print_space fmt ())
             print_field Format.pp_print_space)
          fields
    | Otyp_sum constrs ->
        let print_variant fmt (name, tyl, ret_type_opt) =
          match ret_type_opt with
          | None ->
              if tyl = [] then Format.pp_print_string fmt name
              else
                Format.fprintf fmt "@[<2>%s of@ %a@]"
                  name
                  (list !Oprint.out_type
                     (fun fmt () -> Format.fprintf fmt " *@ "))
                  tyl
          | Some ret_type ->
              if tyl = [] then
                Format.fprintf fmt "@[<2>%s :@ %a@]" name
                  !Oprint.out_type ret_type
              else
                Format.fprintf fmt "@[<2>%s :@ %a -> %a@]"
                  name
                  (list !Oprint.out_type
                     (fun fmt () -> Format.fprintf fmt " *@ "))
                  tyl
                  !Oprint.out_type ret_type
        in
        list print_variant (fun fmt () -> Format.fprintf fmt "@ | ")
          fmt constrs
    | ty ->
        !Oprint.out_type fmt ty

  let out_ty fmt ty =
    let open Outcometree in
    match ty with
    | Osig_class (_,_,_,ctyp,_)
    | Osig_class_type (_,_,_,ctyp,_) ->
        !Oprint.out_class_type fmt ctyp
    | Osig_exception (_,tylst) ->
        list ~paren:true
          !Oprint.out_type
          (fun fmt () ->
            Format.pp_print_char fmt ','; Format.pp_print_space fmt ())
          fmt
          tylst
    | Osig_modtype (_,mtyp)
    | Osig_module (_,mtyp,_) ->
        !Oprint.out_module_type fmt mtyp
    | Osig_type ((_,_,ty,_,_),_) ->
        Format.fprintf fmt "@[<hv 2>%a@]" tydecl ty
    | Osig_value (_,ty,_) ->
        !Oprint.out_type fmt ty

  let ty ?(colorise = no_color) fmt id =
    option_iter id.ty
      (colorise.f Type "%a" fmt out_ty)

  let doc ?colorise:(_ = no_color) fmt id =
    option_iter (Lazy.force id.doc) (Format.fprintf fmt "@[<h>%a@]" lines)

  let loc ?root ?(intf=false) ?colorise:(_ = no_color) fmt id =
    let loc =
      if intf then id.loc_sig
      else Lazy.force id.loc_impl
    in
    if loc = Location.none then
      Format.fprintf fmt "@[<h><no location information>@]"
    else
      let pos = loc.Location.loc_start in
      let fname = match root with
        | Some r when Filename.is_relative pos.Lexing.pos_fname ->
            Filename.concat r pos.Lexing.pos_fname
        | _ -> pos.Lexing.pos_fname
      in
      Format.fprintf fmt "@[<h>%s:%d:%d@]"
        fname pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)

  let info ?(colorise = no_color) fmt id =
    path ~colorise fmt id;
    Format.fprintf fmt " %a" (kind ~colorise) id;
    if id.ty <> None then
      Format.fprintf fmt " @[<h>%a@]" (ty ~colorise) id;
    if Lazy.force id.doc <> None then
      Format.fprintf fmt "@\n    %a" (doc ~colorise) id
end

module Print = struct
  let make (f: ?colorise: IndexFormat.coloriser -> 'a) ?(color=false) id =
    let colorise =
      if color then IndexFormat.color else IndexFormat.no_color
    in
    f ~colorise Format.str_formatter id;
    Format.flush_str_formatter ()

  let name = make IndexFormat.name

  let path = make IndexFormat.path

  let kind = make IndexFormat.kind

  let ty = make IndexFormat.ty

  let doc = make IndexFormat.doc

  let loc ?root ?intf = make (IndexFormat.loc ?root ?intf)

  let info = make IndexFormat.info
end

module Format = IndexFormat
