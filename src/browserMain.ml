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


(** This module contains the run-time for an ncurses interface to ocp-index *)

open Cmdliner

let string_split char str =
  let rec aux pos =
    try
      let i = String.index_from str pos char in
      String.sub str pos (i - pos) :: aux (succ i)
    with Not_found | Invalid_argument _ ->
        let l = String.length str in
        [ String.sub str pos (l - pos) ]
  in
  aux 0

type layout = {
  root: Curses.window;
  input: Curses.window;
  output: Curses.window;
  width: int;
  height:int;
  acs: Curses.Acs.acs;
}

let curses_init color =
  let root = Curses.initscr () in
  let () = at_exit Curses.endwin in
  let _ = Curses.cbreak () in
  let _ = Curses.noecho () in

  let height,width = Curses.getmaxyx root in
  let input = Curses.derwin root 3 width 0 0 in
  let output = Curses.derwin root (height-3) width 3 0 in
  let _ = Curses.keypad input true in

  let acs = Curses.get_acs_codes () in
  (* let () = Curses.scrollok outputwin true in *)
  let _ = Curses.refresh () in
  let _ = Curses.notypeahead () in
  let _init_colors =
    if color then
      let _ = Curses.start_color () in
      for i = 0 to 7 do
        ignore (Curses.init_pair i i Curses.Color.black)
      done
  in
  { root; input; output; width; height; acs }

let kinds_to_string kinds =
  let (@+) (c,b) s = if b then c :: s else s in
  let open IndexOptions in match kinds with
    { t ; v ; e ; c ; m ; s ; k } ->
      let l =
        ("t",t) @+ ("v",v) @+ ("e",e) @+ ("c",c) @+
        ("m",m) @+ ("s",s) @+ ("k",k) @+ []
      in "{ kinds : " ^ String.concat "," l ^ " }"


let clear_input w kinds =
  Curses.werase w.input;
  Curses.box w.input w.acs.Curses.Acs.vline w.acs.Curses.Acs.hline;
  let kindstring = kinds_to_string kinds in
  let _ = Curses.wmove w.input 0 (w.width - (String.length kindstring) - 2) in
  let _ = Curses.waddstr w.input kindstring in
  let _ = Curses.wmove w.input 1 2 in
  ()

type state = {
  query_len: int;
  scroll: int; (* for scrolling: how many lines to skip before printing *)
  completion: (string * int) option;
  (* for completion: what we are completing on, and
     the index of the next completion item to select *)
}

let interactive opts () =
  let w = curses_init opts.IndexOptions.color in
  let query_buf_max = w.width - 4 in
  let query_buf = String.create query_buf_max in
  let rec loop (st:state) =
    let query = String.sub query_buf 0 st.query_len in
    clear_input w opts.IndexOptions.filter ;
    let _ = Curses.waddstr w.input query in
    let ch = Curses.wgetch w.input in
    if st.query_len >= query_buf_max
    then loop st
    else
      let st =
        let nst = { st with scroll = 0;
                            completion = None }
        in
        if ch = Curses.Key.backspace || ch = Curses.Key.left then
          { nst with query_len = max (st.query_len - 1) 0 }
        else if ch = Curses.Key.right || ch = int_of_char '\t' then
          let query, nth = match st.completion with
            | None -> String.sub query_buf 0 st.query_len, 0
            | Some (s,n) -> s, n
          in
          match LibIndex.complete opts.IndexOptions.lib_info query with
          | [] -> st
          | lst ->
              let nb = List.length lst in
              let nth = if nth >= nb then 0 else nth in
              let s = LibIndex.Print.path (List.nth lst nth) in
              let len = min (String.length s) query_buf_max in
              String.blit s 0 query_buf 0 len;
              { nst with completion = Some (query, nth+1);
                         query_len = len }
        else if ch = Curses.Key.npage then
          { nst with scroll = st.scroll + w.height/2 }
        else if ch = Curses.Key.ppage then
          { nst with scroll = max 0 (st.scroll - w.height/2) }
        else if ch = Curses.Key.down then
          { nst with scroll = st.scroll + 1 }
        else if ch = Curses.Key.up then
          { nst with scroll = max 0 (st.scroll - 1) }
        else if ch = 27 then (* The ESC and ALT key, not sure if portable. *)
          (* There is no really sane way to differentiate ESC and ALT and it's not really important anyway, so we don't. *)
          let ch = Curses.wgetch w.input in
          IndexOptions.(
            if ch = (int_of_char 't') then (
              opts.filter <- { opts.filter with t = not opts.filter.t } ;
              st )
            else if ch = (int_of_char 'e') then (
              opts.filter <- { opts.filter with e = not opts.filter.e } ;
              st )
            else if ch = (int_of_char 'c') then (
              opts.filter <- { opts.filter with c = not opts.filter.c } ;
              st )
           else  if ch = (int_of_char 'm') then (
              opts.filter <- { opts.filter with m = not opts.filter.m } ;
              st )
            else if ch = (int_of_char 's') then (
              opts.filter <- { opts.filter with s = not opts.filter.s } ;
              st )
            else if ch = (int_of_char 'k') then (
              opts.filter <- { opts.filter with k = not opts.filter.k } ;
              st )
            else if ch = (int_of_char 'v') then (
              opts.filter <- { opts.filter with v = not opts.filter.v } ;
              st )
            else st
          )
        else
          (query_buf.[st.query_len] <- char_of_int ch;
           { nst with query_len = st.query_len + 1 });
      in
      let query =
        String.sub query_buf 0 st.query_len
      in
      let toskip = ref st.scroll in
      let _set_formatter_ =
        let toprint = ref (w.height - 3) in
        let out str a b =
          if !toskip > 0 then ()
          else if !toprint > 1 then
            for i = a to a + b - 1 do
              ignore (Curses.waddch w.output (int_of_char str.[i]))
            done
        in
        let newline () =
          if !toskip > 0 then decr toskip
          else if !toprint > 2 then
            (out "\n" 0 1; decr toprint)
          else if !toprint > 0 then
            (out "\n..." 0 4; decr toprint)
        in
        let spaces n = out (String.make n ' ') 0 n in
        let flush () = () in
        Format.set_all_formatter_output_functions
          ~out ~flush ~newline ~spaces
      in
      let colorise =
        if not opts.IndexOptions.color then
          LibIndex.Format.no_color
        else
          let attr = function
            | LibIndex.Type -> Curses.WA.color_pair 6
            | LibIndex.Value -> Curses.WA.bold
            | LibIndex.Exception -> Curses.WA.color_pair 3
            | LibIndex.Field _ | LibIndex.Variant _ -> Curses.WA.color_pair 4
            | LibIndex.Method _ -> Curses.WA.bold
            | LibIndex.Module | LibIndex.ModuleType -> Curses.WA.color_pair 1
            | LibIndex.Class | LibIndex.ClassType -> Curses.WA.color_pair 5
            | LibIndex.Keyword -> Curses.WA.color_pair 7
          in
          let _set_tags =
            Format.set_tags true;
            Format.set_formatter_tag_functions {
              Format.
              mark_open_tag =
                (fun a -> Curses.wattron w.output (int_of_string a); "");
              mark_close_tag =
                (fun a -> Curses.wattroff w.output (int_of_string a); "");
              print_open_tag = (fun _ -> ());
              print_close_tag = (fun _ -> ());
            }
          in
          let f kind fstr fmt =
            let tag = string_of_int (attr kind) in
            Format.pp_open_tag fmt tag;
            Format.kfprintf
              (fun fmt ->
                 Format.pp_close_tag fmt ())
              fmt fstr
          in { LibIndex.Format.f }
      in
      let _ = Curses.wclear w.output in
      let response =
        LibIndex.complete
          opts.IndexOptions.lib_info
          ~filter:(IndexOptions.filter opts)
          query
      in
      let _ =
        let fmt = Format.std_formatter in
        List.iter
          (fun id ->
             Format.open_vbox 0 ;
             Format.open_hovbox 4 ;
             LibIndex.Format.kind ~colorise fmt id;
             Format.print_char ' ';
             LibIndex.Format.path ~colorise fmt id;
             (match id with
              | { LibIndex.ty = None }
              | { LibIndex.kind = LibIndex.Module | LibIndex.ModuleType |
                                  LibIndex.Class | LibIndex.ClassType }
                -> ()
              | { LibIndex.ty = Some _ } ->
                  Format.print_space () ;
                  LibIndex.Format.ty ~colorise fmt id );
             Format.close_box ();
             if Lazy.force id.LibIndex.doc <> None then
               (Format.print_break 1 4 ;
                LibIndex.Format.doc ~colorise fmt id);
             Format.close_box ();
             Format.force_newline ())
          response
      in
      let _ = Curses.wrefresh w.output in
      let st = { st with scroll = st.scroll - !toskip } in
      loop st
  in
  loop { query_len = 0; scroll = 0; completion = None }

let browser_cmd : unit Term.t * Term.info =
  let doc = "Interactively completes and prints documentation." in
  Term.(pure interactive $ IndexOptions.common_opts $ pure ()),
  Term.info "ocp-browser" ~doc

let () =
  match Term.eval browser_cmd
  with
  | `Error _ -> exit 1
  | _ -> exit 0
