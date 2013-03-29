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

type t = (char, id) Trie.t
and id = { path: string list;
           kind: kind;
           name: string;
           ty: Types.type_expr option;
           doc: string;
           (* library: string option *) }
and kind =
  | Type | Value | Exception
  | Field of id | Variant of id
  | Method
  | Module | ModuleType
  | Class | ClassType

let string_to_list s =
  let rec aux acc i = if i >= 0 then aux (s.[i]::acc) (i - 1) else acc in
  aux [] (String.length s - 1)

let _list_to_string l =
  let rec aux n = function
    | [] -> String.create n
    | c::r -> let s = aux (n+1) r in s.[n] <- c; s
  in
  aux 0 l

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

(* Tries to locate an ocamldoc comment associated with location loc in a list of comments
   (string * loc). Rough approximation, don't expect accuracy wrt ocamldoc binding heuristics.
   (The function doesn't have enough info for that anyway)
*)
let associate_comment comments loc =
  let lstart = loc.Location.loc_start.Lexing.pos_lnum
  and lend =  loc.Location.loc_end.Lexing.pos_lnum in
  let rec aux = function
    | [] -> None
    | (comment, cloc)::comments ->
        let cstart = cloc.Location.loc_start.Lexing.pos_lnum
        and cend =  cloc.Location.loc_end.Lexing.pos_lnum in
        if cend < lstart - 1 then aux comments
        else if cstart > lend + 1 then None
        else if String.length comment = 0 || comment.[0] <> '*' then aux comments
        else
          let comment = String.trim (String.sub comment 1 (String.length comment - 1)) in
          match aux comments with
          | None -> Some comment
          | Some c -> Some (Printf.sprintf "%s\n%s" comment c)
  in
  aux comments

let rec trie_of_sig_item ?(comments=[]) path = function
  | Types.Sig_value (id,descr) ->
      string_to_list id.Ident.name,
      Trie.create ~value:{
        path = path;
        kind = Value;
        name = id.Ident.name;
        ty = Some descr.Types.val_type;
        doc =
          match associate_comment comments descr.Types.val_loc
          with Some s -> s | None -> "";
      } ()
  | Types.Sig_type (id,descr,_rec) ->
      string_to_list id.Ident.name,
        let t =
          Trie.create ~value:{
            path = path;
            kind = Type;
            name = id.Ident.name;
            ty = descr.Types.type_manifest;
            doc =
              match associate_comment comments descr.Types.type_loc
              with Some s -> s | None -> "";
          } ()
        in t
  | Types.Sig_exception (id,descr) ->
      string_to_list id.Ident.name,
      Trie.create ~value:{
        path = path;
        kind = Exception;
        name = id.Ident.name;
        ty = (match descr.Types.exn_args with
          | [] -> Some {Types. desc = Types.Tnil; id=(-1); level=(-1)}
          | [t] -> Some t
          | t::_ as ts ->
              Some { Types. desc = Types.Ttuple ts;
                     id = t.Types.id; level = t.Types.level });
        doc = "";
      } ()
  | Types.Sig_module (id,Types.Mty_signature sign,_)
  | Types.Sig_modtype (id,Types.Modtype_manifest (Types.Mty_signature sign))
    as s ->
      string_to_list id.Ident.name,
      let t =
        Trie.create
          ~value:{
            path = path;
            kind = (match s with Types.Sig_module _ -> Module
                               | Types.Sig_modtype _ -> ModuleType
                               | _ -> assert false);
            name = id.Ident.name;
            ty = None;
            doc = "";
          } ()
      in
      Trie.graft t ['.']
        (List.fold_left
           (fun t sign ->
             let k, v = trie_of_sig_item (path@[id.Ident.name]) sign in
             Trie.graft t k v)
           Trie.empty
           sign)
  | Types.Sig_module (id,_,_)
  | Types.Sig_modtype (id,_)
    as s ->
      (* abstract modtype, type alias or functor;
         for aliases, we need lookup at some point.
         For now, only add the name *)
      string_to_list id.Ident.name,
      Trie.create ~value:{
        path = path;
        kind = (match s with Types.Sig_module _ -> Module
                           | Types.Sig_modtype _ -> ModuleType
                           | _ -> assert false);
        name = id.Ident.name;
        ty = None;
        doc = "";
      } ()
  | Types.Sig_class (id,_,_rec)
  | Types.Sig_class_type (id,_,_rec)
    as s -> (* todo *)
      string_to_list id.Ident.name,
      Trie.create ~value:{
        path = path;
        kind = (match s with Types.Sig_class _ -> Class
                           | Types.Sig_class_type _ -> ClassType
                           | _ -> assert false);
        name = id.Ident.name;
        ty = None;
        doc = "";
      } ()

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
          doc = ""
        }
      in
      Trie.graft_lazy t ['.'] (lazy (
        Printf.eprintf "\027[31mOpening CMI %s\027[m\n%!" file;
        let info = Cmi_format.read_cmi file in
        List.fold_left
          (fun t sign ->
            let k, v = trie_of_sig_item [modul] sign in
            Trie.graft t k v)
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
          doc = ""
        }
      in
      Trie.graft_lazy t ['.'] (lazy (
        Printf.eprintf "\027[31mOpening CMT %s\027[m\n%!" file;
        let info = Cmt_format.read_cmt file in
        let comments = info.Cmt_format.cmt_comments in
        match info.Cmt_format.cmt_annots with
        | Cmt_format.Interface sign ->
            List.fold_left
              (fun t sign ->
                let k, v = trie_of_sig_item ~comments [modul] sign in
                Trie.graft t k v)
              Trie.empty
              (sign.Typedtree.sig_type)


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
      Printtyp.reset_names ();
      Printtyp.type_expr Format.str_formatter ty;
      Format.flush_str_formatter ()
  | None -> ""

let format_ty fmt ty =
  Printtyp.reset_names ();
  Printtyp.type_expr fmt ty

let doc _ = assert false
let loc _ = assert false

let all t =
  trie_to_list t

(* Trie.fold (fun key opt acc -> if opt <> None then key::acc else acc) t [] *)

let option_iter opt f = match opt with
  | Some x -> f x
  | None -> ()

let format_id ?(color=true) fmt id =
  let colM, colV, colT, col0 = 31, 32, 36, 0 in
  let color =
    if color then fun fmt c ->
      Format.fprintf fmt "@<0>%s" (Printf.sprintf "\027[%dm" c)
    else fun _ _ -> ()
  in
  let print_path () =
    List.iter (fun m -> Format.fprintf fmt "%a%s%a." color colM m color col0) id.path
  in
  match id.kind with
  | Module ->
      print_path ();
      Format.fprintf fmt "%a%s%a" color colM id.name color col0
  | Value ->
      print_path ();
      Format.fprintf fmt "%a%s%a" color colV id.name color col0;
      option_iter id.ty (fun ty ->
        Format.fprintf fmt " : @[<h>%a%a%a@]" color colT format_ty ty color col0
      );
      if id.doc <> "" then
        Format.fprintf fmt "@\n    @[<h4>%s@]" id.doc
  | Type ->
      print_path ();
      Format.fprintf fmt "%a%s%a" color colT id.name color col0;
      option_iter id.ty (fun ty ->
        Format.fprintf fmt " = @[<h>%a%a%a@]" color colT format_ty ty color col0
      )
  | _ -> ()

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
