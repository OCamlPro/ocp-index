open Lwt
open Lwt_react
open LTerm_widget
open CamomileLibraryDyn.Camomile

(* LibIndex.info contains lazy values, we need a specialized equality. *)
let rec eq l1 l2 = match l1, l2 with
  | [], [] -> true
  | [] , _::_  | _::_ , [] -> false
  | {LibIndex. path = path1 ; name = name1 } :: t1 ,
    {LibIndex. path = path2 ; name = name2 } :: t2 ->
      path1 = path2 && name1 = name2 && eq t1 t2

(** Provide an association LibIndex.kind -> string -> style
   In order to encode styles in [Format.tag]. *)
(* This is absolutely horrible, but I don't know how to do better *)
let get_attr, attr_tbl =
  let bold = LTerm_style.({ none with bold = Some true}) in
  let colindex i = LTerm_style.({ none with foreground = Some (index i)}) in
  let h = Hashtbl.create 11 in
  let attr = function
    | LibIndex.Type -> "Type"
    | Value -> "Value"
    | Exception -> "Exception"
    | Field _  -> "Field"
    | Variant _ -> "Variant"
    | Method _ -> "Method"
    | Module -> "Module"
    | ModuleType -> "ModuleType"
    | Class -> "Class"
    | ClassType -> "ClassType"
    | Keyword -> "Keyword"
  in
  Hashtbl.add h "Type"       @@ colindex 6 ;
  Hashtbl.add h "Value"      @@ bold ;
  Hashtbl.add h "Exception"  @@ colindex 3 ;
  Hashtbl.add h "Field"      @@ colindex 4 ;
  Hashtbl.add h "Variant"    @@ colindex 4 ;
  Hashtbl.add h "Method"     @@ bold ;
  Hashtbl.add h "Module"     @@ colindex 1 ;
  Hashtbl.add h "ModuleType" @@ colindex 1 ;
  Hashtbl.add h "Class"      @@ colindex 5 ;
  Hashtbl.add h "ClassType"  @@ colindex 5 ;
  Hashtbl.add h "Keyword"    @@ colindex 7 ;
  Hashtbl.add h "Enabled"    @@ LTerm_style.({ none with underline = Some true}) ;
  Hashtbl.add h "Disabled"   @@ colindex 8 ;
  attr, h

(** Create a custom styled text formater. *)
(* Should go into lambda-term at some point. *)
let make_fmt () =
  let style = Stack.create () in
  let content = ref [||] in

  let get_style () =
    if Stack.is_empty style then LTerm_style.none
    else Stack.top style
  and pop_style () =
    if Stack.is_empty style then ()
    else ignore (Stack.pop style)
  and push_style sty =
    if Stack.is_empty style then Stack.push sty style
    else Stack.push (LTerm_style.merge (Stack.top style) sty) style
  in

  let put s pos len =
    let s = String.sub s pos len in
    content := Array.append !content (LTerm_text.stylise s (get_style ()))
  in
  let flush () = () in
  let fmt = Format.make_formatter put flush in

  let get_content () =
    Format.pp_print_flush fmt () ; !content
  in

  Format.pp_set_tags fmt true;
  Format.pp_set_formatter_tag_functions fmt {
    Format.
    mark_open_tag =
      (fun a -> push_style (Hashtbl.find attr_tbl a) ; "");
    mark_close_tag =
      (fun _ -> pop_style (); "");
    print_open_tag = (fun _ -> ());
    print_close_tag = (fun _ -> ());
  } ;

  get_content, fmt

let pp_with_style to_style =
  fun style fstr fmt ->
    let tag = to_style style in
    Format.pp_open_tag fmt tag;
    Format.kfprintf
      (fun fmt ->
         Format.pp_close_tag fmt ())
      fmt fstr

let colorise opts =
  if not opts.IndexOptions.color then
    LibIndex.Format.no_color
  else
    let f kind fstr fmt = pp_with_style get_attr kind fstr fmt
    in { LibIndex.Format.f }

(** Format the complete answer and return a styled text. *)
let sprint_answer ?(doc=false) cols colorise id =
  let get_content, fmt = make_fmt () in
  Format.pp_set_margin fmt cols ;
  let print = Format.fprintf fmt in

  print "@[<hv 4>" ;
  LibIndex.Format.kind ~colorise fmt id;
  print " ";
  LibIndex.Format.path ~short:true ~colorise fmt id;
  begin match id with
    | { LibIndex.ty = None }
    | { LibIndex.kind = LibIndex.Module | LibIndex.ModuleType |
                        LibIndex.Class | LibIndex.ClassType }
      -> ()
    | { LibIndex.ty = Some _ } ->
        print "@ @[<h>" ;
        LibIndex.Format.ty ~colorise fmt id;
        print "@]" ;
  end ;
  if doc && Lazy.force id.LibIndex.doc <> None
  then begin
    print "@\n    " ;
    LibIndex.Format.doc ~colorise fmt id
  end ;
  print "@]" ;
  Format.pp_print_flush fmt () ;
  get_content ()


(** Key Bindings *)

module Bindings = Zed_input.Make (LTerm_key)

let () =
  let open LTerm_read_line in
  let open LTerm_key in
  let edit x = Edit (LTerm_edit.Zed x) in
  bind [{ control = false; meta = false; shift = false; code = Right }]    [edit Next_char];
  bind [{ control = false; meta = false; shift = false; code = Left }]     [edit Prev_char];
  bind [{ control = false; meta = true; shift = false; code = Backspace }] [edit Delete_prev_word];

  bind [{ control = false; meta = false; shift = false; code = Prev_page }] [Complete_bar_first];
  bind [{ control = false; meta = false; shift = false; code = Next_page }]  [Complete_bar_last];
  bind [{ control = false; meta = false; shift = false; code = Up }]   [Complete_bar_prev];
  bind [{ control = false; meta = false; shift = false; code = Down }] [Complete_bar_next];

  bind [{ control = false; meta = true; shift = false; code = Up }]    [Complete_bar_prev];
  bind [{ control = false; meta = true; shift = false; code = Down }]  [Complete_bar_next];
  bind [{ control = false; meta = true; shift = false; code = Right }] [Complete_bar];
  (* bind [{ control = false; meta = true; shift = false; code = Left }]  [edit Delete_prev_word]; *)
  (* Not defined here, because what we want is not exactly a command. *)

  bind [{ control = false; meta = false; shift = false; code = Enter }] [Complete_bar];

  (* We use Alt+c to toggle constructors. *)
  LTerm_edit.unbind [{ control = false ; meta = true ; shift = false ; code = Char (UChar.of_char 'c')}] ;
  ()


(** Line editor *)
(* Delicate mix between LTerm_read_line.engine and LTerm_edit.edit *)
(* Should go into lambda-term. *)
let newline = UChar.of_char '\n'


class virtual line_editor = object(self)
  inherit LTerm_widget.t "edit"
  inherit [Zed_rope.t] LTerm_read_line.engine () as super

  method text = Zed_rope.to_string (Zed_edit.text self#edit)

  val mutable style = LTerm_style.none
  val mutable marked_style = LTerm_style.none
  val mutable current_line_style = LTerm_style.none
  method! update_resources =
    let rc = self#resource_class and resources = self#resources in
    style <- LTerm_resources.get_style rc resources;
    marked_style <- LTerm_resources.get_style (rc ^ ".marked") resources;
    current_line_style <- LTerm_resources.get_style (rc ^ ".current-line") resources

  val mutable event = E.never
  val mutable resolver = None

  method! can_focus = true

  initializer
    event <- E.map (fun _ -> self#queue_draw) (Zed_edit.update self#edit [Zed_edit.cursor self#context]);
    self#on_event
      (function
         | LTerm_event.Key key -> begin
             let res =
               match resolver with
               | Some res -> res
               | None -> Bindings.resolver [
                   Bindings.pack (fun x -> x) !LTerm_read_line.bindings;
                   Bindings.pack (List.map (fun x -> LTerm_read_line.Edit x)) !LTerm_edit.bindings]
             in
             match Bindings.resolve key res with
               | Bindings.Accepted actions ->
                   resolver <- None;
                   List.iter self#send_action actions ;
                   true
               | Bindings.Continue res ->
                   resolver <- Some res;
                   true
               | Bindings.Rejected ->
                   if resolver = None then
                     match key with
                       | { control = false; meta = false; shift = false; code = Char ch } ->
                           Zed_macro.add self#macro (Edit (LTerm_edit.Zed (Zed_edit.Insert ch)));
                           self#insert ch ;
                           true
                       | _ ->
                           false
                   else begin
                     resolver <- None;
                     false
                   end
           end
         | _ ->
             false)

  method! send_action = function
    | Edit (Zed Newline) -> ()
    | action -> super#send_action action

  val mutable shift = 0
  val mutable start = 0

  method! draw ctx _focused =
    let open LTerm_draw in

    let size = LTerm_draw.size ctx in

    (*** Check that the cursor is displayed ***)

    let line_set = Zed_edit.lines self#edit in
    let cursor_offset = Zed_cursor.get_position (Zed_edit.cursor self#context) in
    let cursor_line = Zed_lines.line_index line_set cursor_offset in
    let cursor_column = cursor_offset - Zed_lines.line_start line_set cursor_line in

    (* Horizontal check *)
    if cursor_column < shift || cursor_column >= shift + size.cols then
      shift <- max 0 (cursor_column - size.cols / 2);

    (* Vertical check *)
    let start_line = Zed_lines.line_index line_set start in
    let start_line =
      if cursor_line < start_line || cursor_line >= start_line + size.rows then begin
        let start_line = max 0 (cursor_line - size.rows / 2) in
        start <- Zed_lines.line_start line_set start_line;
        start_line
      end else
        start_line
    in

    (*** Drawing ***)

    (* Initialises points with the text style and spaces. *)
    fill ctx (UChar.of_char ' ');
    fill_style ctx style;

    (*** Text drawing ***)

    let rec draw_line row col zip =
      if Zed_rope.Zip.at_eos zip then
        draw_eoi (row + 1)
      else
        let char, zip = Zed_rope.Zip.next zip in
        if char = newline then begin
          let row = row + 1 in
          if row < size.rows then begin_line row zip
        end else begin
          if col > size.cols then begin
            let row = row + 1 in
            if row < size.rows then skip_eol row zip
          end else begin
            draw_char ctx row col char;
            draw_line row (col + 1) zip
          end
        end

    and skip_eol row zip =
      if Zed_rope.Zip.at_eos zip then
        draw_eoi (row + 1)
      else
        let char, zip = Zed_rope.Zip.next zip in
        if char = newline then
          begin_line row zip
        else
          skip_eol row zip

    and skip_bol row zip remaining =
      if remaining = 0 then
        draw_line row 0 zip
      else if Zed_rope.Zip.at_eos zip then
        draw_eoi (row + 1)
      else
        let char, zip = Zed_rope.Zip.next zip in
        if char = newline then begin
          let row = row + 1 in
          if row < size.rows then begin_line row zip
        end else
          skip_bol row zip (remaining - 1)

    and begin_line row zip =
      if Zed_rope.Zip.at_eos zip then
        draw_eoi row
      else if shift <> 0 then begin
        skip_bol row zip shift
      end else
        draw_line row 0 zip

    and draw_eoi _row =
      ()
    in

    let text = Zed_edit.text self#edit in

    begin_line 0 (Zed_rope.Zip.make_f text start);

    (* Colorize the current line. *)
    for col = 0 to size.cols - 1 do
      set_style (point ctx (cursor_line - start_line) col) current_line_style
    done;

    (* Colorize the selection if needed *)
    if Zed_edit.get_selection self#edit then begin
      let sel_offset = Zed_cursor.get_position (Zed_edit.mark self#edit) in
      let sel_line = Zed_lines.line_index line_set sel_offset in
      let sel_column = sel_offset - Zed_lines.line_start line_set sel_line in
      let line_a, column_a, line_b, column_b =
        if sel_offset < cursor_offset then
          (sel_line, sel_column, cursor_line, cursor_column)
        else
          (cursor_line, cursor_column, sel_line, sel_column)
      in
      let line_a, column_a =
        if line_a < start_line then
          (start_line, 0)
        else
          (line_a, column_a)
      in
      let line_b, column_b =
        if line_b >= start_line + size.rows then
          (start_line + size.rows - 1, size.cols - 1)
        else
          (line_b, column_b)
      in
      if line_a < start_line + size.rows && line_b >= start_line then begin
        let line_a = line_a - start_line and line_b = line_b - start_line in
        let column_a = column_a and column_b = column_b in
        if line_a = line_b then
          for column = column_a to column_b - 1 do
            set_style (point ctx line_a column) marked_style
          done
        else begin
          for column = column_a to size.cols - 1 do
            set_style (point ctx line_a column) marked_style
          done;
          for line = line_a + 1 to line_b - 1 do
            for column = 0 to size.cols - 1 do
              set_style (point ctx line column) marked_style
            done
          done;
          for column = 0 to column_b - 1 do
            set_style (point ctx line_b column) marked_style
          done
        end
      end
    end

  method! cursor_position =
    let line_set = Zed_edit.lines self#edit in
    let cursor_offset = Zed_cursor.get_position (Zed_edit.cursor self#context) in
    let cursor_line = Zed_lines.line_index line_set cursor_offset in
    let cursor_column = cursor_offset - Zed_lines.line_start line_set cursor_line in
    let start_line = Zed_lines.line_index line_set start in
    Some { row = cursor_line - start_line; col = cursor_column - shift }
end


(** Strip one path level.

    Do the following transformation:
    "Foo.Bar."    -> "Foo."
    "Foo.Bar.bla" -> "Foo.Bar."
*)
(* Currently, it computes where to cut, go to the end of the text, erase the extra text, replace the cursor.
   It's not atomic and will fire multiple useless events.
*)
let strip_path_level text context =
  if Zed_rope.is_empty text then ()
  else begin
    let module Z = Zed_rope.Zip in
    let dot = CamomileLibrary.UChar.of_char '.' in
    (* If the last char is a dot, we want to skip it, otherwise, we don't care.*)
    let zip = Z.make_b text 1 in
    let i = Z.(offset (find_b ( (==) dot) zip)) in
    let len = Zed_rope.length text in
    let previous_pos = Zed_edit.position context in

    Zed_edit.goto_eot context ;
    Zed_edit.remove_prev context (len - i) ;
    Zed_edit.goto context (min i previous_pos)
  end

(** Mono line input with completion for a LibIndex.path. *)
class completion_box options wakener =

  let completion_info, set_completion_info =
    S.create ~eq ([] : LibIndex.info list) in

  object (self)
    inherit line_editor as super

    val size_request = { LTerm_geom. rows = 1; cols = 1 }
    method! size_request = size_request

    method eval = Zed_edit.text self#edit

    initializer
      self#on_event
      (function
        | LTerm_event.Key { control = false; meta = true; shift = false; code = Left } ->
            strip_path_level (Zed_edit.text self#edit) self#context ;
            true
        | _ -> false
      )

    method! completion =
      let content = self#eval in
      let response =
        LibIndex.complete
          options.IndexOptions.lib_info
          ~filter:(IndexOptions.filter options)
          (Zed_rope.to_string content)
      in
      set_completion_info response ;
      let completions =
        let suffix = function
          | {LibIndex. kind = Module | ModuleType } -> "."
          | {LibIndex. kind = Class | ClassType } -> "#"
          | _ -> ""
        in
        List.map
          (fun x -> LibIndex.Print.path ~short:true x, suffix x)
          response
      in
      self#set_completion 0 completions

    method completion_info = completion_info

    (** Only insert chars that are valid in OCaml identifiers. *)
    method! insert ch =
      let is_valid_char x = try match UChar.char_of x with
        | 'a'..'z' | 'A'..'Z' | '_' | '\'' | '#'
        | '!'|'$'|'%'|'&'|'*'|'+'|'-'|'.'|'/'
        | ':'|'<'|'='|'>'|'?'|'@'|'^'|'|'|'~'
          -> true
        | _ -> false
        with UChar.Out_of_range -> false
      in
      if is_valid_char ch then super#insert ch
      else ()

    method! send_action = function
      (* Exit the app on Break and Interrupt *)
      | action ->
          try super#send_action action
          with Sys.Break | LTerm_read_line.Interrupt -> wakeup wakener ()

  end


(** Count the number of line took by a text. *)
(* Assume there are now overfills, should be ensured by format. *)
let height (str : LTerm_text.t) =
  let last = Array.length str - 1 in
  let count = ref 0 in
  for i = 0 to last do
    if fst str.(i) = newline then incr count
  done ;
  (* Don't count a potential last newline twice *)
  if fst str.(last) <> newline then incr count ;
  !count

(** The show box shows the result of a research.

    [content] is a list zipper positioned at the focused element.
    left and right lists are elements before and after the focus.

    We want to draw a focused element, as in the middle as possible, at [default_pos].
    We don't want to format more left and right elements than necessary.

    We first format the left elements, in right-to-left order, until the height of formatted
    texts is more than [default_pos]. We then format right elements in left-to-right order
    until the total height of formatted text is longer than the number of rows. We may need to
    go back to the left elements, if there are not enough right elements.

    We then render everything in the same fashion, with the focused element at
    [max 0 (min size_left default_pos)].
*)
class show_box color = object (self)
  inherit LTerm_widget.t "show_box"

  val mutable content = [], []

  method content = content

  method set_content new_content =
    content <- new_content ;
    self#queue_draw

  method! draw ctx _focused =
    let {LTerm_geom. rows ; cols } = LTerm_geom.size_of_rect self#allocation in
    let cols = cols - 2 in
    match content with
    | _, [] -> ()
    | left, focus :: right -> begin
        let text_focus = sprint_answer ~doc:true cols color focus in
        let size_focus = height text_focus in
        let default_pos = (rows - size_focus) / 2 in

        (* Can't figure out how to do simpler, bear with me. *)
        let rec format (dir:[ `L | `R ]) left right size_l size_r format_l format_r =
          match dir, left, right with
          (* We are done (no more to draw or too much drawn already). *)
          | _, [], [] -> size_l, size_r, List.rev format_l, List.rev format_r
          | _, _ , _ when size_r + size_focus + size_l > rows ->
              size_l, size_r, List.rev format_l, List.rev format_r

          (* Finished the left part, and stuff to do on the right. *)
          | `L , [], _::_ -> format `R left right size_l size_r format_l format_r
          | `L , _ , _::_ when size_l > default_pos ->
              format `R left right size_l size_r format_l format_r

          (* The right part is too short, go back to the left. *)
          | `R, _::_, [] ->
              format `L left right size_l size_r format_l format_r

          | `L, info :: t, _ | `R, _, info :: t ->
              let text = sprint_answer cols color info in
              let size = height text in
              if dir = `L then
                format dir t right (size + size_l) size_r ((text,size) :: format_l) format_r
              else
                format dir left t size_l (size + size_r) format_l ((text,size) :: format_r)
        in
        let size_left, right_size, formatted_left, formatted_right =
          format `L left right 0 0 [] []
        in

        let rec draw_left pos = function
          | [] -> ()
          | (text, size) :: t ->
              LTerm_draw.draw_styled ctx (pos-size) 2 text ; draw_left (pos-size) t
        in
        let rec draw_right pos = function
          | [] -> ()
          | (text, size) :: t ->
              LTerm_draw.draw_styled ctx pos 2 text ; draw_right (pos + size) t
        in

        let start = max 0 (min size_left (max default_pos (rows - right_size - size_focus))) in

        draw_left start formatted_left ;
        LTerm_draw.draw_styled ctx start 2 text_focus ;
        LTerm_draw.draw_char ctx start 0 @@ CamomileLibrary.UChar.of_char '>' ;
        draw_right (start + size_focus) formatted_right
      end

end


(* Only in ocaml >= 4.02 *)
let rec pp_print_list ?(pp_sep = Format.pp_print_cut) pp_v ppf = function
  | [] -> ()
  | [v] -> pp_v ppf v
  | v :: vs ->
    pp_v ppf v;
    pp_sep ppf ();
    pp_print_list ~pp_sep pp_v ppf vs

(** Pretty printer for kinds with colors. *)
let pp_kinds fmt options =
  let pp_kind fmt (c, hash, b) =
    if b then (
      Format.pp_open_tag fmt "Enabled" ;
      pp_with_style (fun x -> x) hash "%s" fmt c ;
      Format.pp_close_tag fmt ()
    ) else
      pp_with_style (fun x -> x) "Disabled" "%s" fmt c ;
  in
  let open IndexOptions in
  let { t ; v ; e ; c ; m ; s ; k } = options.filter in
  let l = [
    ("t","Type",t) ; ("v","Value",v) ; ("e","Exception",e) ;
    ("c","Variant",c) ; ("m","Module",m) ; ("s","ModuleType",s) ;
    ("k","Keyword",k) ] in
  let pp_sep fmt () = Format.fprintf fmt ", " in
  Format.fprintf fmt " kinds: %a " (pp_print_list ~pp_sep pp_kind) l

(** A frame with extra info on the border. *)
class frame_info options = object
  inherit frame as super

  method! draw ctx focused =
    super#draw ctx focused ;
    let get_content, fmt = make_fmt () in
    pp_kinds fmt options ;
    let s = get_content () in
    let width = (LTerm_draw.size ctx).cols in
    let len = Array.length s in
    if width > len + 2 then
      LTerm_draw.draw_styled ctx 0 (width - len - 1) s
end

let change_kind completion_box shox_box options = function
  | LTerm_event.Key { control = false; meta = true; shift = false; code = Char ch } ->
      let open IndexOptions in
      let fil = options.filter in
      let new_fil =
        if ch = UChar.of_char 't'      then { fil with t = not fil.t }
        else if ch = UChar.of_char 'e' then { fil with e = not fil.e }
        else if ch = UChar.of_char 'c' then { fil with c = not fil.c }
        else if ch = UChar.of_char 'm' then { fil with m = not fil.m }
        else if ch = UChar.of_char 's' then { fil with s = not fil.s }
        else if ch = UChar.of_char 'k' then { fil with k = not fil.k }
        else if ch = UChar.of_char 'v' then { fil with v = not fil.v }
        else fil
      in options.filter <- new_fil ;
      completion_box#completion ;
      shox_box#queue_draw ;
      true
  | _ -> false

(** Express the result as an event mapped on the content of the completion box. *)
let show_completion show_box input =
  let zipper n l =
    let rec aux k acc = function
      | h :: l when k > 0 -> aux (k-1) (h::acc) l
      | l -> acc, l
    in aux n [] l
  in
  let eq_pair (l1, l2) (l1', l2') = eq l1 l1' && eq l2 l2' in

  input#completion_info
  |> S.l2 ~eq:eq_pair zipper input#completion_index
  |> S.map show_box#set_content


(** Boilerplate *)

let main options =
  let waiter, wakener = wait () in

  let root = new LTerm_widget.vbox in
  let comp = new frame_info options in
  let input = new completion_box options wakener in
  comp#set input ;
  root#add ~expand:false comp ;

  let show_box = new show_box (colorise options) in
  root#add show_box ;

  root#on_event (change_kind input show_box options) ;

  S.keep @@ show_completion show_box input ;

  Lazy.force LTerm.stdout >>=
  fun term -> LTerm_widget.run term root waiter

let run options () =
  Lwt_main.run (main options)

let main_term : unit Cmdliner.Term.t * Cmdliner.Term.info =
  let open Cmdliner in
  let doc = "Interactively completes and prints documentation." in
  Term.(pure run $ IndexOptions.common_opts $ pure ()),
  Term.info "ocp-browser" ~doc

let () =
  match Cmdliner.Term.eval main_term
  with
  | `Error _ -> exit 1
  | _ -> exit 0
