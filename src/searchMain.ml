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

module I = LibIndex
module SP = Spelll

module Search : sig
  type t

  val make : I.t -> t

  val search : t -> string -> I.info list
end = struct
  type t = I.t

  let make i = i

  let search (idx:t) k =
    let dfa = SP.of_string ~limit:1 k in
    I.fold_all idx
      (fun acc info ->
         if SP.match_with dfa info.I.name
         then info::acc else acc)
      []
end

let search options name =
  let i = Search.make options.IndexOptions.lib_info in
  let l = Search.search i name in
  List.iter
    (fun info -> print_endline (I.Print.info info))
    l

let () =
  let open Cmdliner in
  let doc = "Find identifiers of OCaml libraries that are close to the current \
             query term" in
  let man = [ ] in
  let name_opt =
    let doc = "name of the identifier that is looked for" in
    Arg.(value & pos 0 string "" & info [] ~docv:"NAME" ~doc)
  in
  match
    Term.eval
      (Term.(pure search $ IndexOptions.common_opts () $ name_opt),
       Term.info "ocp-search" ~version:"1.1.5" ~doc ~man)
  with
  | `Ok () -> exit 0
  | `Error _ -> exit 2
  | _ -> exit 0

(* idea: single utility to color parts of source with syntactic context:
   pattern, expr, type, topexpr, module, record ...
   Could be used for better completion, analysis etc.*)

