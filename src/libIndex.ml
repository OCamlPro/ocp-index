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
              doc: string option;
              file: orig_file;
           (* library: string option *) }

and kind =
  | Type | Value | Exception
  | Field of info | Variant of info
  | Method of info
  | Module | ModuleType
  | Class | ClassType

type t = (char, info) Trie.t

(* - Utility functions - *)

let string_to_list s =
  let rec aux acc i = if i >= 0 then aux (s.[i]::acc) (i - 1) else acc in
  aux [] (String.length s - 1)

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
        if Sys.is_directory path then subdirs acc path else acc)
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

let open_module ?(cleanup_path=false) t path =
  let f =
    if cleanup_path then
      let n = List.length path in
      let rec tln n = function
        | [] -> [] | _::tl -> if n > 0 then tln (n-1) tl else tl
      in
      fun id -> {id with path = tln n id.path}
    else
      fun id -> id
  in
  Trie.fold
    (fun t path id -> Trie.add t path (f id))
    (Trie.sub t
       (List.fold_right (fun p acc -> string_to_list p @ '.' :: acc) path []))
    t

(* Pops comments from a list of comments (string * loc) to find the ones that
   are associated to a given location. Also returns the remaining comments after
   the location. *)
let associate_comment ?(after_only=false) comments loc =
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

let trie_of_type_decl ?(comments=[]) info ty_decl =
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
            doc = None;
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
            doc = None;
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
            List.find (fun item -> kind = Module &&
                                   modul = (id_of_sig_item item).Ident.name)
              sign
          in
          match modul with
          | Types.Sig_module (_,Types.Mty_signature sign,_) ->
              find_item path sign
          | _ -> raise Not_found
    in
    let cmt_contents = Cmt_format.read_cmt cmt in
    let sign =
      match cmt_contents.Cmt_format.cmt_annots with
      | Cmt_format.Implementation impl -> List.rev impl.Typedtree.str_type
      | _ -> raise Not_found
    in
    let item = find_item path sign in
    loc_of_sig_item item
  with
  | Not_found -> Location.none

let rec trie_of_sig_item ?(comments=[]) orig_file path sig_item =
  let id = id_of_sig_item sig_item in
  let loc = loc_of_sig_item sig_item in
  let doc, comments =
    if loc = Location.none then None, comments
    else associate_comment comments loc
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
        trie_of_type_decl ~comments info descr
    | _ -> [], comments
  in
  let children, comments = (* read module / class contents *)
    match sig_item with
    | Types.Sig_module (id,Types.Mty_signature sign,_)
    | Types.Sig_modtype (id,Types.Modtype_manifest (Types.Mty_signature sign))
      ->
        let path = path @ [id.Ident.name] in
        List.fold_left
          (fun (t,comments) sign ->
            let chlds,comments =
              trie_of_sig_item ~comments orig_file path sign
            in
            List.fold_left Trie.append t chlds, comments)
          (Trie.empty,comments)
          sign
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
        List.fold_left (fun t (lbl,_,ty_expr) ->
          if lbl = "*dummy method*" then t else
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
                doc = None;
                file = info.file })
          Trie.empty
          fields,
        comments
    | _ -> Trie.empty, comments
  in
  let name = id.Ident.name in
  if String.length name > 0 && name.[0] = '#' then [], comments
  else
    (string_to_list id.Ident.name,
     Trie.create
       ~value:info
       ~children:(lazy ['.', children])
       ())
    :: siblings,
    comments

let load_cmi t modul orig_file =
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
          doc = None;
          file = orig_file;
        }
      in
      let children = lazy (
        let info = Cmi_format.read_cmi (orig_file_name orig_file) in
        List.fold_left
          (fun t sig_item ->
            let chld, _comments = trie_of_sig_item orig_file [modul] sig_item in
            List.fold_left Trie.append t chld)
          Trie.empty
          info.Cmi_format.cmi_sign
      ) in
      Trie.graft_lazy t ['.'] children
    )

let load_cmt t modul orig_file =
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
          doc = None;
          file = orig_file;
        }
      in
      let children = lazy (
        let info = Cmt_format.read_cmt (orig_file_name orig_file) in
        let comments = info.Cmt_format.cmt_comments in
        match info.Cmt_format.cmt_annots with
        | Cmt_format.Implementation {Typedtree.str_type = sign; _}
        | Cmt_format.Interface {Typedtree.sig_type = sign; _}
          ->
            let t, _trailing_comments =
              List.fold_left
                (fun (t,comments) sig_item ->
                   let chld, comments =
                     trie_of_sig_item ~comments orig_file [modul] sig_item
                   in
                   List.fold_left Trie.append t chld, comments)
                (Trie.empty, comments)
                sign
            in
            t
        | _ ->
            Printf.eprintf "\027[33mWarning: unhandled cmt format\027[m\n%!";
            t
      )
      in
      Trie.graft_lazy t ['.'] children
    )

let load_file t modul f = match f with
  | Cmi _ -> load_cmi t modul f
  | Cmt _ | Cmti _ -> load_cmt t modul f

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
          load_file t modul file)
    modules
    t

let load_dir t dir =
  let files = Array.to_list (Sys.readdir dir) in
  load_files t dir files

let load paths =
  let t =
    List.fold_left load_dir (Trie.create ()) paths
  in
  open_module ~cleanup_path:true t ["Pervasives"]


(* - Query functions - *)

let filter_visible values =
  let same_kind a b = match a.kind, b.kind with
    | Field i, Field j | Variant i, Variant j | Method i, Method j ->
        i.name = j.name
    | a, b -> a = b
  in
  let rev =
    List.fold_left
      (fun acc info ->
        if List.exists (fun n -> same_kind info n) acc
        then acc else info::acc)
      [] values
  in
  List.rev rev

let trie_to_list trie =
  Trie.fold0
    (fun acc _path values -> (filter_visible values) @ acc)
    trie []

let all t =
  trie_to_list t

let filter t f =
  Trie.fold0
    (fun acc _path values ->
       (filter_visible (List.filter f values)) @ acc)
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
    option_iter id.doc (Format.fprintf fmt "@[<h>%a@]" lines)

  let loc ?colorise:(_ = no_color) fmt id =
    let loc = Lazy.force id.loc_impl in
    (* let loc = id.loc_sig in *)
    if loc = Location.none then
      Format.fprintf fmt "@[<h><no location information>@]"
    else
      let pos = loc.Location.loc_start in
      Format.fprintf fmt "@[<h>%s:%d:%d@]"
        pos.Lexing.pos_fname pos.Lexing.pos_lnum
        (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)

  let info ?(colorise = no_color) fmt id =
    path ~colorise fmt id;
    Format.fprintf fmt " %a" (kind ~colorise) id;
    if id.ty <> None then
      Format.fprintf fmt " @[<h>%a@]" (ty ~colorise) id;
    if id.doc <> None then
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

  let loc = make IndexFormat.loc

  let info = make IndexFormat.info
end

module Format = IndexFormat
