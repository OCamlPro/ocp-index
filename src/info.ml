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
           doc: string }
and kind =
  | Type | Value | Exception
  | Field | Variant
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
      fun id -> {id with path = tln n id.path }
    else
      fun id -> id
  in
  Trie.fold
    (fun t path id -> Trie.set t path (f id))
    (Trie.sub t
       (List.fold_right (fun p acc -> string_to_list p @ '.' :: acc) path []))
    t

let rec load_module path sign =
  List.map (function
    | Types.Sig_value (id,descr) ->
        string_to_list id.Ident.name,
        Trie.create ~value:{
          path = path;
          kind = Value;
          name = id.Ident.name;
          ty = Some descr.Types.val_type;
          doc = "";
        } ()
    | Types.Sig_type (id,descr,_rec) ->
        string_to_list id.Ident.name,
        Trie.create ~value:{
          path = path;
          kind = Type;
          name = id.Ident.name;
          ty = descr.Types.type_manifest;
          doc = "";
        } ()
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
             (fun t (k,n) -> Trie.graft t k n)
             Trie.empty
             (load_module (path@[id.Ident.name]) sign))
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
        } ())
    sign

let load paths =
  let t =
    List.fold_left (fun t path ->
      Array.fold_left (fun t file ->
        if Filename.check_suffix file ".cmi" then
          let modul =
            String.capitalize (Filename.chop_extension (Filename.basename file))
          in
          let children = lazy (
            let info = Cmi_format.read_cmi (Filename.concat path file) in
            assert (modul = info.Cmi_format.cmi_name);
            List.fold_left (fun t (key,v) -> Trie.graft t key v)
              Trie.empty
              (load_module [modul] info.Cmi_format.cmi_sign)
          ) in
          Trie.map_subtree t (string_to_list modul)
            (fun t ->
              if Trie.sub t ['.'] <> Trie.empty then
                Printf.eprintf
                  "[33mWarning: module %s defined more than once[m\n%!"
                  modul;
              let t = Trie.graft_lazy t ['.'] children in
              Trie.set t [] {
                path = [];
                kind = Module;
                name = modul;
                ty = None;
                doc = ""
              }
            )
        else t)
      t
      (Sys.readdir path))
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
      )
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
      Printf.sprintf "\027[%dm%s\027[m"
        (match c with `red -> 31
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
