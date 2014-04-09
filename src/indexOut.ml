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

open IndexTypes

let option_iter opt f = match opt with
  | Some x -> f x
  | None -> ()

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
        | Keyword -> "\027[32m"
      in
      Format.pp_print_as fmt 0 colorcode;
      Format.kfprintf (fun fmt -> Format.pp_print_as fmt 0 "\027[m") fmt fstr
    in { f }

  let no_color =
    let f _ fstr fmt = Format.fprintf fmt fstr in
    { f }

  let name ?(colorise = no_color) fmt id =
    colorise.f id.kind "%s" fmt id.name

  let path ?(short = false) ?(colorise = no_color) fmt id =
    List.iter
      (Format.fprintf fmt "%a." (colorise.f Module "%s"))
      (if short then id.path else id.orig_path);
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
    | Keyword -> Format.pp_print_string fmt "keyword"

  let rec tydecl fmt =
    let open Outcometree in
    function
    | Otyp_abstract -> Format.fprintf fmt "<abstract>"
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
    | Otyp_sum [] ->
        Format.pp_print_char fmt '-'
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
    | Osig_exception (_,[]) ->
        Format.pp_print_char fmt '-'
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

  let file ?colorise:(_ = no_color) fmt id =
    Format.fprintf fmt "@[<h>%s@]"
      (match id.file with Cmt f | Cmi f | Cmti f -> f)

  let info ?(colorise = no_color) fmt id =
    path ~colorise fmt id;
    Format.fprintf fmt " %a" (kind ~colorise) id;
    if id.ty <> None then
      Format.fprintf fmt " @[<h>%a@]" (ty ~colorise) id;
    if Lazy.force id.doc <> None then
      Format.fprintf fmt "@\n    %a" (doc ~colorise) id

  let format ?root format ?colorise fmt id =
    let rec aux i =
      let j =
        try String.index_from format i '%'
        with Not_found -> String.length format
      in
      if j > i then Format.fprintf fmt "%s" (String.sub format i (j - i));
      if j < String.length format - 1 then
        let () = match format.[j+1] with
          | 'n' -> name ?colorise fmt id
          | 'q' -> path ~short:true ?colorise fmt id
          | 'p' -> path ?colorise fmt id
          | 'k' -> kind ?colorise fmt id
          | 't' -> ty   ?colorise fmt id
          | 'd' -> doc  ?colorise fmt id
          | 'l' -> loc  ?root ?colorise fmt id
          | 's' -> loc  ?root ~intf:true ?colorise fmt id
          | 'f' -> file ?colorise fmt id
          | 'i' -> info ?colorise fmt id
          | '%' -> Format.fprintf fmt "%%"
          | c   -> Format.fprintf fmt "%%%c" c
        in
        aux (j + 2)
      else if j < String.length format then
        Format.fprintf fmt "%s"
          (String.sub format j (String.length format - j))
    in
    aux 0
end

module Print = struct
  let make (f: ?colorise: IndexFormat.coloriser -> 'a) ?(color=false) id =
    let colorise =
      if color then IndexFormat.color else IndexFormat.no_color
    in
    f ~colorise Format.str_formatter id;
    Format.flush_str_formatter ()

  let name = make IndexFormat.name

  let path ?short = make (IndexFormat.path ?short)

  let kind = make IndexFormat.kind

  let ty = make IndexFormat.ty

  let doc = make IndexFormat.doc

  let loc ?root ?intf = make (IndexFormat.loc ?root ?intf)

  let file = make IndexFormat.file

  let info = make IndexFormat.info

  let format ?root format = make (IndexFormat.format ?root format)
end

module Format = IndexFormat
