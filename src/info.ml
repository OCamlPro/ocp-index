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

let option_default d = function
  | Some x -> x
  | None -> d


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

let ty_of_sig_item sig_item =
  match Printtyp.tree_of_signature [sig_item] with
  | [] -> None
  | ty::_ -> Some ty

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

(*
  | Types.Tpoly (ty, tylist) ->
      List.fold_left
        (fun t ty -> match ty.Types.desc with
          | Types.Tconstr (Path.Pident id, params, _) ->
              Printf.eprintf "\027[32mBingo !\027[m\n%!";
              Trie.graft t (string_to_list id.Ident.name)
                (trie_of_type (path@[id.Ident.name]) (Types.Ttuple params))
          | _ -> Printf.eprintf "\027[31mTPoly -> Tconstr : miss !\027[m\n%!"; t)
        Trie.empty
        (ty :: tylist)
  | _ ->
      Printf.eprintf "\027[31m%s -> unhandled\027[m\n%!" (String.concat "." path);
      Trie.empty
*)

let trie_of_sig_item ?(comments=[]) path sig_item =
  let id = id_of_sig_item sig_item in
  let loc = loc_of_sig_item sig_item in
  let doc, comments =
    if loc = Location.none then None, comments
    else associate_comment comments loc
  in
  let ty = ty_of_sig_item sig_item in
  let kind = kind_of_sig_item sig_item in
  let info = {path; kind; name = id.Ident.name; ty; loc; doc} in
  let siblings, comments = (* read fields / labels ... *)
    match sig_item with
    | Types.Sig_type (_id,descr,_is_rec) ->
        trie_of_type_decl ~comments info descr
    | _ -> [], comments
  in
  let children, comments = (* read module / class contents (todo) *)
    lazy [], comments
(*
      let t =
        (match ty with
         | None -> t
         | Some ty ->
             Trie.graft t ['.'] (trie_of_type (path @ [id.Ident.name]) ty.Types.desc))
      in
  | Types.Sig_module (id,(Types.Mty_signature sign as modtype),_)
  | Types.Sig_modtype (id,Types.Modtype_manifest (Types.Mty_signature sign as modtype))
    as s ->
      let subtree, comments =
        List.fold_left
          (fun (t,comments) sign ->
            let k, v, comments =
              trie_of_sig_item ~comments (path@[id.Ident.name]) sign in
            Trie.graft t k v, comments)
          (Trie.empty,comments)
          sign
      in
      let t = Trie.graft t ['.'] subtree in
  in
*)
  in
  (string_to_list id.Ident.name,
   Trie.create
     ~value:{path; kind; name = id.Ident.name; ty; loc; doc}
     ~children:(lazy ['.', Trie.create ~children ()])
     ())
  :: siblings,
  comments

let load_cmi t modul file =
  Trie.map_subtree t (string_to_list modul)
    (fun t ->
      if Trie.sub t ['.'] <> Trie.empty then
        Printf.eprintf
          "[33mWarning: module %s defined more than once[m\n%!"
          modul;
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
      Trie.graft_lazy t ['.'] (lazy (
        let info = Cmi_format.read_cmi file in
        List.fold_left
          (fun t sign ->
            let chld, _comments = trie_of_sig_item [modul] sign in
            List.fold_left
              (fun t (k,v) -> Trie.graft t k v)
              t
              chld)
          Trie.empty
          (info.Cmi_format.cmi_sign)
      ))
    )

let load_cmt t modul file =
  Trie.map_subtree t (string_to_list modul)
    (fun t ->
      if Trie.sub t ['.'] <> Trie.empty then
        Printf.eprintf
          "[33mWarning: module %s defined more than once[m\n%!"
          modul;
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
      Trie.graft_lazy t ['.'] (lazy (
        let info = Cmt_format.read_cmt file in
        let comments = info.Cmt_format.cmt_comments in
        match info.Cmt_format.cmt_annots with
        | Cmt_format.Interface sign ->
            let t, _remaining_comments =
              List.fold_left
                (fun (t,comments) sign ->
                  let chld, comments = trie_of_sig_item ~comments [modul] sign in
                  List.fold_left (fun t (k,v) -> Trie.graft t k v) t chld,
                  comments)
                (Trie.empty, comments)
                (sign.Typedtree.sig_type)
            in
            t

            (* let types = *)
            (*   List.map *)
            (*     Typedtree.(function *)
            (*       | {sig_desc = Tsig_value (id,_loc,v)} -> *)
            (*          (\* v.val_val.Types.val_type; *\) t *)
            (*       | x ->  t *)

(* | 	Tsig_type of (Ident.t * string Asttypes.loc * type_declaration) list *)
(* | 	Tsig_exception of Ident.t * string Asttypes.loc * exception_declaration *)
(* | 	Tsig_module of Ident.t * string Asttypes.loc * module_type *)
(* | 	Tsig_recmodule of (Ident.t * string Asttypes.loc * module_type) list *)
(* | 	Tsig_modtype of Ident.t * string Asttypes.loc * modtype_declaration *)
(* | 	Tsig_open of Path.t * Longident.t Asttypes.loc *)
(* | 	Tsig_include of module_type * Types.signature *)
(* | 	Tsig_class of class_description list *)
(* | 	Tsig_class_type of class_type_declaration list *)
            (*     ) *)
            (*     sign.Typedtree.sig_items *)
            (* in *)
            (* t *)
        | _ ->
            Printf.eprintf "\027[33mWarning: unhandled cmti format\027[m\n%!";
            t
            (* List.fold_left (fun t (key,v) -> Trie.graft t key v) *)
            (*   Trie.empty *)
            (*   (Cmt_format.read_cmt file) *)
      ))
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
        | Type -> 36
        | Value -> 32
        | Exception -> 33
        | Field _ | Variant _ -> 34
        | Method _ -> 32
        | Module | ModuleType -> 31
        | Class | ClassType -> 35
      in
      Format.pp_print_as fmt 0 (Printf.sprintf "\027[%dm" colorcode);
      Format.kfprintf (fun fmt -> Format.pp_print_as fmt 0 "\027[m") fmt fstr
    else fun _ fstr fmt ->
      Format.fprintf fmt fstr
  in
  List.iter (Format.fprintf fmt "%a." (colorise Module "%s")) id.path;
  colorise id.kind "%s" fmt id.name;
  option_iter id.ty
    (Format.fprintf fmt " @[<h>%a@]" (fun fmt -> colorise Type "%a" fmt format_ty));
  let str_kind = match id.kind with
    | Type -> "type"
    | Value -> "val"
    | Exception -> "exception"
    | Field parentty -> Printf.sprintf "field(%s)" parentty.name
    | Variant parentty -> Printf.sprintf "constr(%s)" parentty.name
    | Method parentclass -> Printf.sprintf "method(%s)" parentclass.name
    | Module -> "module"
    | ModuleType -> "modtype"
    | Class -> "class"
    | ClassType -> "classtype"
  in
  Format.fprintf fmt " <%s>" str_kind;
  option_iter id.doc (Format.fprintf fmt "@\n    @[<h4>%s@]")


let pretty ?(color=true) id =
  let color =
    if not color then fun _ () s -> s
    else fun c () ->
      Printf.sprintf "\027[%dm%s\027[m" (match c with `red -> 31
                                                    | `green -> 32
                                                    | `blue -> 36)
  in
  match id.kind with
  | Module ->
      String.concat "." (List.map (color `red ()) (id.path @ [id.name]))
  | Value ->
      String.concat "."
        (List.map (color `red ()) id.path @ [color `green () id.name])
  | Type ->
      String.concat "."
        (List.map (color `red ()) id.path @ [color `blue () id.name])
  | _ -> ""
