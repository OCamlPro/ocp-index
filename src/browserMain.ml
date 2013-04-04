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

let clear_input w =
  Curses.werase w.input;
  Curses.box w.input w.acs.Curses.Acs.vline w.acs.Curses.Acs.hline;
  let _ = Curses.wmove w.input 1 2 in
  ()

let rec skipn l n =
  if n = 0 then l else match l with
    | _::r -> skipn r (n-1)
    | [] -> []

let rec curseprint win attr str =
  let len = String.length str in
  let i = try String.index str '\027' with Not_found -> len in
  let _ = Curses.waddstr win (String.sub str 0 i) in
  Curses.wattr_off win attr;
  if i < len then
    if String.sub str (i+1) 2 = "[m" then
      let attr = Curses.WA.normal in
      Curses.wattron win attr;
      curseprint win attr (String.sub str (i+3) (len - i - 3))
    else
      let str = String.sub str (i+1) (len - i - 1) in
      Scanf.sscanf str
        "[%um%n"
        (fun color i ->
          let attr =
            if color = 1 then Curses.WA.bold
            else Curses.WA.color_pair (color - 30)
          in
          Curses.wattron win attr;
          curseprint win attr (String.sub str i (String.length str - i)))
  else attr

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
    clear_input w;
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
          match Info.complete opts.IndexOptions.lib_info query with
          | [] -> st
          | lst ->
              let nb = List.length lst in
              let nth = if nth >= nb then 0 else nth in
              let s = Info.name (List.nth lst nth) in
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
        else
          (query_buf.[st.query_len] <- char_of_int ch;
           { nst with query_len = st.query_len + 1 });
      in
      let query =
        String.sub query_buf 0 st.query_len
      in
      let response_str =
        let fmt = Format.str_formatter in
        List.iter
          (fun id ->
            Info.format_info ~color:opts.IndexOptions.color fmt id;
            Format.pp_force_newline fmt ())
          (Info.complete opts.IndexOptions.lib_info query);
        Format.flush_str_formatter ()
      in
      let lines = string_split '\n' response_str in
      let lines = skipn lines st.scroll in
      let _ = Curses.wclear w.output in
      let max_y = w.height - 4 in
      let rec pr attr = function
        | line::r ->
            let y,_ = Curses.getyx w.output in
            if y < max_y then
              (Curses.wattron w.output attr;
               let attr = curseprint w.output attr line in
               let _ = Curses.waddch w.output (int_of_char '\n') in
               pr attr r)
        | [] -> ()
      in
      let _ = pr Curses.WA.normal lines in
      let _ = Curses.wrefresh w.output in
      loop st
  in
  loop { query_len = 0; scroll = 0; completion = None }

let interactive_cmd : unit Term.t * Term.info =
  let doc = "Interactively completes and prints documentation." in
  Term.(pure interactive $ IndexOptions.common_opts $ pure ()),
  Term.info "interactive" ~doc

let () =
  match Term.eval interactive_cmd
  with
  | `Error _ -> exit 1
  | _ -> exit 0
