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

let open_module t path =
  Trie.fold_paths
    (fun t path v -> Trie.set t (List.rev path) v)
    (Trie.sub t (string_to_list (path ^ ".")))
    t

let rec load_module t path sign =
  let mk_key =
    let rev_keypath =
      '.' :: List.rev (string_to_list (String.concat "." path))
    in
    fun s -> List.rev_append rev_keypath (string_to_list s)
  in
  let t =
    List.fold_left (fun t -> function
      | Types.Sig_value (id,descr) ->
          Trie.set t
            (mk_key id.Ident.name)
            { path = path;
              kind = Value;
              name = id.Ident.name;
              ty = Some descr.Types.val_type;
              doc = ""; }
      | Types.Sig_type (id,descr,_rec) ->
          Trie.set t
            (mk_key id.Ident.name)
            { path = path;
              kind = Type;
              name = id.Ident.name;
              ty = descr.Types.type_manifest;
              doc = ""; }
      | Types.Sig_exception (id,descr) ->
          Trie.set t
            (mk_key id.Ident.name)
            { path = path;
              kind = Exception;
              name = id.Ident.name;
              ty = (match descr.Types.exn_args with
                | [] -> Some {Types. desc = Types.Tnil; id=(-1); level=(-1)}
                | [t] -> Some t
                | t::_ as ts ->
                    Some { Types. desc = Types.Ttuple ts;
                           id = t.Types.id; level = t.Types.level });
              doc = ""; }
      | Types.Sig_module (id,Types.Mty_signature sign,_)
      | Types.Sig_modtype (id,Types.Modtype_manifest (Types.Mty_signature sign))
        as s ->
          let t =
            Trie.set t
              (mk_key id.Ident.name)
              { path = path;
                kind = (match s with Types.Sig_module _ -> Module
                                   | Types.Sig_modtype _ -> ModuleType
                                   | _ -> assert false);
                name = id.Ident.name;
                ty = None;
                doc = ""; }
          in
          load_module t (path@[id.Ident.name]) sign
      | Types.Sig_module (id,_,_)
      | Types.Sig_modtype (id,_)
        as s ->
          (* abstract modtype, type alias or functor;
             for aliases, we need lookup at some point.
             For now, only add the name *)
          Trie.set t
            (mk_key id.Ident.name)
            { path = path;
              kind = (match s with Types.Sig_module _ -> Module
                                 | Types.Sig_modtype _ -> ModuleType
                                 | _ -> assert false);
              name = id.Ident.name;
              ty = None;
              doc = ""; }
      | Types.Sig_class (id,_,_rec)
      | Types.Sig_class_type (id,_,_rec)
        as s -> (* todo *)
          Trie.set t
            (mk_key id.Ident.name)
            { path = path;
              kind = (match s with Types.Sig_class _ -> Class
                                 | Types.Sig_class_type _ -> ClassType
                                 | _ -> assert false);
              name = id.Ident.name;
              ty = None;
              doc = ""; })
      t
      sign
  in
  open_module t "Pervasives"

let load paths =
  List.fold_left (fun t path ->
    Array.fold_left (fun t file ->
      if Filename.check_suffix file ".cmi" then
        let module C = Cmi_format in
        let modul =
          String.capitalize (Filename.chop_extension (Filename.basename file))
        in
        let t =
          Trie.set t
            (string_to_list modul)
            { path = [];
              kind = Module;
              name = modul;
              ty = None;
              doc = "" }
        in
        let info = C.read_cmi (Filename.concat path file) in
        assert (modul = info.C.cmi_name);
        load_module t [modul] info.C.cmi_sign
      else t)
      t
      (Sys.readdir path))
    (Trie.create ())
    paths

let trie_to_list trie =
  Trie.fold_paths (fun acc _path value -> value::acc) trie []

let complete t query =
  trie_to_list
    (Trie.filter ((<>) '.')
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

let doc _ = assert false
let loc _ = assert false

let all t =
  trie_to_list t

  (* Trie.fold (fun key opt acc -> if opt <> None then key::acc else acc) t [] *)

let pretty id =
  match id.kind with
  | Module ->
      Printf.sprintf "\027[31m%s\027[m\n"
        (String.concat "." (id.path @ [id.name]))
  | Value ->
      Printf.sprintf "\027[31m%s\027[m.\027[32m%s\027[m: \027[36m%s\027[m\n"
        (String.concat "\027[m.\027[31m" id.path)
        id.name
        (ty id)
  | Type ->
      Printf.sprintf "\027[31m%s\027[m.\027[36m%s\027[m\n"
        (String.concat "\027[m.\027[31m" id.path)
        id.name
  | _ -> ""
