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

type info = { path: string list;
              kind: kind;
              name: string;
              ty: Outcometree.out_sig_item option;
              loc: Location.t;
              doc: string option;
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

let _list_to_string l =
  let rec aux n = function
    | [] -> String.create n
    | c::r -> let s = aux (n+1) r in s.[n] <- c; s
  in
  aux 0 l

let option_iter opt f = match opt with
  | Some x -> f x
  | None -> ()


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
    (fun t path id -> Trie.set t path (f id))
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
        else if String.length comment < 2 || comment.[0] <> '*' || comment.[1] = '*' then
          aux comments
        else
          let comment = String.trim (String.sub comment 1 (String.length comment - 1)) in
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
            loc = Location.none;
            doc = None
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
            loc = Location.none;
            doc = None
          } ())
        variants,
      comments

let rec trie_of_sig_item ?(comments=[]) path sig_item =
  let id = id_of_sig_item sig_item in
  let loc = loc_of_sig_item sig_item in
  let doc, comments =
    if loc = Location.none then None, comments
    else associate_comment comments loc
  in
  let ty = Some (ty_of_sig_item sig_item) in
  let kind = kind_of_sig_item sig_item in
  let info = {path; kind; name = id.Ident.name; ty; loc; doc} in
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
        List.fold_left
          (fun (t,comments) sign ->
            let chlds,comments =
              trie_of_sig_item ~comments (path@[id.Ident.name]) sign
            in
            List.fold_left Trie.append t chlds, comments)
          (Trie.empty,comments)
          sign
(* WIP
    | Types.Sig_class (id,{Types.cty_type=cty},_)
    | Types.Sig_class_type (id,{Types.clty_type=cty},_)
      ->
        let rec get_clsig = function
          | Types.Cty_constr (_,_,cty) | Types.Cty_fun (_,_,cty) ->
              get_clsig cty
          | Types.Cty_signature clsig -> clsig
        in
        let tylst = match (get_clsig cty).Types.cty_self.Types.desc with
          | Types.Tobject (_, {contents = Some tylst}) -> tylst
          | _ -> assert false
        in
        let path = path@[id.Ident.name]
        List.fold_left
          (fun (t,comments) sign ->
            Trie.set t (string_to_list id.Ident.name) {
              path;
              kind = ;
              name = modul;
              ty = None;
              loc = Location.in_file file;
              doc = None }

            let chlds,comments =
              trie_of_type ~comments (path@[id.Ident.name]) sign
            in
            List.fold_left (fun t (k,v) -> Trie.graft t k v) t chlds, comments)
          (Trie.empty,comments)
          tylst
*)
    | _ -> Trie.empty, comments
  in
  (string_to_list id.Ident.name,
   Trie.create
     ~value:{path; kind; name = id.Ident.name; ty; loc; doc}
     ~children:(lazy ['.', children])
     ())
  :: siblings,
  comments

let load_cmi t modul file =
  Trie.map_subtree t (string_to_list modul)
    (fun t ->
      let t =
        Trie.set t [] {
          path = [];
          kind = Module;
          name = modul;
          ty = None;
          loc = Location.in_file file;
          doc = None
        }
      in
      let children = lazy (
        let info = Cmi_format.read_cmi file in
        List.fold_left
          (fun t sig_item ->
            let chld, _comments = trie_of_sig_item [modul] sig_item in
            List.fold_left Trie.append t chld)
          Trie.empty
          info.Cmi_format.cmi_sign
      ) in
      Trie.graft_lazy t ['.'] children
    )

let load_cmt t modul file =
  Trie.map_subtree t (string_to_list modul)
    (fun t ->
      let t =
        Trie.set t [] {
          path = [];
          kind = Module;
          name = modul;
          ty = None;
          loc = Location.in_file file;
          doc = None
        }
      in
      let children = lazy (
        let info = Cmt_format.read_cmt file in
        let comments = info.Cmt_format.cmt_comments in
        match info.Cmt_format.cmt_annots with
        | Cmt_format.Interface sign ->
            let t, _remaining_comments =
              List.fold_left
                (fun (t,comments) sig_item ->
                  let chld, comments =
                    trie_of_sig_item ~comments [modul] sig_item
                  in
                  List.fold_left Trie.append t chld, comments)
                (Trie.empty, comments)
                (sign.Typedtree.sig_type)
            in
            t
        | _ ->
            Printf.eprintf "\027[33mWarning: unhandled cmti format\027[m\n%!";
            t
      )
      in
      Trie.graft_lazy t ['.'] children
    )

let load paths =
  let t =
    List.fold_left (fun t path ->
      let files = Sys.readdir path in
      let has_cmti base =
        let n = base ^ ".cmti" in
        let rec aux i = i >= 0 && (files.(i) = n || aux (i-1)) in
        aux (Array.length files - 1)
      in
      Array.fold_left (fun t file ->
        let basename, extension = try
            let i = String.rindex file '.' in
            let len = String.length file in
            String.sub file 0 i, String.sub file (i+1) (len-i-1)
          with Not_found -> file, ""
        in
        let modul = String.capitalize basename in
        match extension with
        | "cmi" when not (has_cmti basename) ->
            load_cmi t modul (Filename.concat path file)
        | "cmti" ->
            load_cmt t modul (Filename.concat path file)
        | _ -> t)
      t
      files)
    (Trie.create ())
    paths
  in
  open_module ~cleanup_path:true t ["Pervasives"]

(* - Output functions - *)

let trie_to_list trie =
  Trie.fold (fun acc _path value -> value::acc) trie []

let get t query = Trie.find t (string_to_list query)

let complete t query =
  trie_to_list
    (Trie.filter_keys ((<>) '.')
       (Trie.sub t (string_to_list query)))

let complete_module _ = assert false

let complete_value _ = assert false

let complete_type _ = assert false

let complete_class _ = assert false

let name id = String.concat "." (id.path @ [id.name])

let ty id =
  match id.ty with
  | Some ty ->
      !Oprint.out_sig_item Format.str_formatter ty;
      Format.flush_str_formatter ()
  | None -> ""

let format_list fmt
    ?(paren=false) ?(left=fun _ -> ()) ?(right=fun _ -> ())
    pr lst sep
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

let format_lines fmt str =
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

let format_ty fmt ty =
  match ty with
  | Outcometree.Osig_class (_,_,_,ctyp,_)
  | Outcometree.Osig_class_type (_,_,_,ctyp,_) ->
      !Oprint.out_class_type fmt ctyp
  | Outcometree.Osig_exception (_,tylst) ->
      format_list fmt ~paren:true
        !Oprint.out_type tylst
        (fun fmt () ->
          Format.pp_print_char fmt ','; Format.pp_print_space fmt ())
  | Outcometree.Osig_modtype (_,mtyp)
  | Outcometree.Osig_module (_,mtyp,_) ->
      !Oprint.out_module_type fmt mtyp
  | Outcometree.Osig_type ((_,_,ty,_,_),_) ->
      !Oprint.out_type fmt ty
  | Outcometree.Osig_value (_,ty,_) ->
      !Oprint.out_type fmt ty

let doc _ = assert false
let loc _ = assert false

let all t =
  trie_to_list t

(* Trie.fold (fun key opt acc -> if opt <> None then key::acc else acc) t [] *)

let format_info ?(color=true) fmt id =
  let colorise =
    if color then fun kind fstr fmt ->
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
    else fun _ fstr fmt ->
      Format.fprintf fmt fstr
  in
  List.iter (Format.fprintf fmt "%a." (colorise Module "%s")) id.path;
  colorise id.kind "%s" fmt id.name;
  option_iter id.ty
    (Format.fprintf fmt " @[<h>%a@]" (fun fmt -> colorise Type "%a" fmt format_ty));
  let format_kind fmt = function
    | Type -> Format.pp_print_string fmt "type"
    | Value -> Format.pp_print_string fmt "val"
    | Exception -> Format.pp_print_string fmt "exception"
    | Field parentty ->
        Format.fprintf fmt "field(%a)" (colorise Type "%s") parentty.name
    | Variant parentty ->
        Format.fprintf fmt "constr(%a)" (colorise Type "%s") parentty.name
    | Method parentclass ->
        Format.fprintf fmt "method(%a)" (colorise Class "%s") parentclass.name
    | Module -> Format.pp_print_string fmt "module"
    | ModuleType -> Format.pp_print_string fmt "modtype"
    | Class -> Format.pp_print_string fmt "class"
    | ClassType -> Format.pp_print_string fmt "classtype"
  in
  Format.fprintf fmt " <%a>" format_kind id.kind;
  option_iter id.doc (Format.fprintf fmt "@\n    @[<h>%a@]" format_lines)
