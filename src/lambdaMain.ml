open Lwt
open Lwt_react
open LTerm_widget
open CamomileLibraryDyn.Camomile

exception Exit

(** This is absolutely horrible, but I don't know how to do better *)
(* Provide an association LibIndex.kind -> string -> style
   In order to encode styles in [Format.tag]. *)
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
  attr, h

(** Create a custom styled text formater. *)
let make_fmt () =
  let style = ref LTerm_style.none in
  let content = ref [||] in
  let get_content () = !content in

  let put s pos len =
    let s = String.sub s pos len in
    content := Array.append !content (LTerm_text.stylise s !style)
  in
  let flush () = () in
  let fmt = Format.make_formatter put flush in

  Format.pp_set_tags fmt true;
  Format.pp_set_formatter_tag_functions fmt {
    Format.
    mark_open_tag =
      (fun a -> style := Hashtbl.find attr_tbl a ; "");
    mark_close_tag =
      (fun _ -> style := LTerm_style.none; "");
    print_open_tag = (fun _ -> ());
    print_close_tag = (fun _ -> ());
  } ;

  get_content, fmt


let colorise opts =
  if not opts.IndexOptions.color then
    LibIndex.Format.no_color
  else
    let f kind fstr fmt =
      let tag = get_attr kind in
      Format.pp_open_tag fmt tag;
      Format.kfprintf
        (fun fmt ->
           Format.pp_close_tag fmt ())
        fmt fstr
    in { LibIndex.Format.f }

(** Format the complete answer and return a styled text. *)
let sprint_answer cols colorise id =
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
  if Lazy.force id.LibIndex.doc <> None
  then begin
    print "@\n    " ;
    LibIndex.Format.doc ~colorise fmt id
  end ;
  print "@]" ;
  Format.pp_print_flush fmt () ;
  get_content ()


(** Key Bindings *)

module Bindings = Zed_input.Make (LTerm_key)

let unbind_all seq = LTerm_edit.unbind seq ; LTerm_read_line.unbind seq

let () =
  let open LTerm_read_line in
  let open LTerm_key in
  let edit x = Edit (LTerm_edit.Zed x) in
  bind [{ control = false; meta = false; shift = false; code = Right }]    [edit Next_char];
  bind [{ control = false; meta = false; shift = false; code = Left }]     [edit Prev_char];
  bind [{ control = false; meta = true; shift = false; code = Right }]     [edit Next_word];
  bind [{ control = false; meta = true; shift = false; code = Left }]      [edit Prev_word];
  bind [{ control = false; meta = true; shift = false; code = Backspace }] [edit Kill_prev_word];

  bind [{ control = false; meta = false; shift = false; code = Up }]   [Complete_bar_prev];
  bind [{ control = false; meta = false; shift = false; code = Down }] [Complete_bar_next];

  bind [{ control = false; meta = false; shift = false; code = Enter }] [Complete_bar];
  ()


let regexp_word =
  let set = UCharInfo.load_property_set `Alphabetic in
  let set = List.fold_left (fun set ch -> USet.add (UChar.of_char ch) set) set ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'] in
  Zed_re.compile (`Repn(`Set set, 1, None))
let newline = UChar.of_char '\n'


class virtual line_editor =
  let key_press, new_key_press = E.create () in

object(self)
  inherit LTerm_widget.t "edit"
  inherit [Zed_rope.t] LTerm_read_line.engine () as super

  method text = Zed_rope.to_string (Zed_edit.text self#edit)

  val mutable style = LTerm_style.none
  val mutable marked_style = LTerm_style.none
  val mutable current_line_style = LTerm_style.none
  method update_resources =
    let rc = self#resource_class and resources = self#resources in
    style <- LTerm_resources.get_style rc resources;
    marked_style <- LTerm_resources.get_style (rc ^ ".marked") resources;
    current_line_style <- LTerm_resources.get_style (rc ^ ".current-line") resources

  val mutable event = E.never
  val mutable resolver = None

  method can_focus = true

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
                           Zed_edit.insert self#context (Zed_rope.singleton ch);
                           new_key_press () ;
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

  method key_press = key_press

  method! send_action = function
    | Edit (Zed Newline) -> ()
    | action -> super#send_action action

  val mutable shift = 0
  val mutable start = 0

  method draw ctx focused =
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

    and draw_eoi row =
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

  method cursor_position =
    let line_set = Zed_edit.lines self#edit in
    let cursor_offset = Zed_cursor.get_position (Zed_edit.cursor self#context) in
    let cursor_line = Zed_lines.line_index line_set cursor_offset in
    let cursor_column = cursor_offset - Zed_lines.line_start line_set cursor_line in
    let start_line = Zed_lines.line_index line_set start in
    Some { row = cursor_line - start_line; col = cursor_column - shift }
end



class completion_box options wakener =

  let (completion_info : LibIndex.info list React.event), set_completion_info
    = E.create () in

  object (self)
    inherit line_editor as super

    val size_request = { LTerm_geom. rows = 1; cols = 1 }
    method! size_request = size_request

    method eval = Zed_edit.text self#edit

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
        let is_module = function
          | {LibIndex. kind = Module | ModuleType | Class | ClassType } -> true
          | _ -> false
        in
        List.map
          (fun x ->
             let dot = if is_module x then "." else "" in
             LibIndex.Format.path ~short:true Format.str_formatter x ;
             (Format.flush_str_formatter (), dot) )
          response
      in
      self#set_completion 0 completions

    method completion_info = completion_info


    method! send_action = function
      (* Exit the app on Break and Interrupt *)
      | action ->
          try super#send_action action
          with Sys.Break | LTerm_read_line.Interrupt -> wakeup wakener ()

  end

let text_size (str : LTerm_text.t) =
  let rec loop ofs rows cols max_cols =
    if ofs = Array.length str then
      {LTerm_geom. rows; cols = max cols max_cols }
    else
      let chr = str.(ofs) in
      let ofs = ofs + 1 in
      if fst chr = newline then
        if ofs = Array.length str then
          {LTerm_geom. rows; cols = max cols max_cols }
        else
          loop ofs (rows + 1) 0 (max cols max_cols)
      else
        loop ofs rows (cols + 1) max_cols
  in
  loop 0 1 0 0



(** Widgets ! *)

(** Like label, for styled text *)
(* should be contributed to lambda-term *)
class styled_label initial_text = object(self)
  inherit t "stylized_label"
  val mutable text = initial_text

  val mutable size_request = text_size initial_text
  method size_request = size_request

  method text = text
  method set_text t =
    text <- t;
    size_request <- text_size t;
    self#queue_draw

  method draw ctx focused =
    let {LTerm_geom. rows } = LTerm_draw.size ctx in
    LTerm_draw.draw_styled ctx 0 0 text
end

class show_box = object (self)
  inherit LTerm_widget.vbox

  method add_label s =
    let l = new styled_label s in
    self#add ~expand:false l

  method clean =
    List.iter self#remove self#children

end

(** Events *)

let completion_event options =
  let color = colorise options in
  fun showbox comp_list ->
    let {LTerm_geom. cols } = LTerm_geom.(size_of_rect showbox#allocation) in
    showbox#clean ;
    List.iter
      (fun id ->
         let s = sprint_answer cols color id in
         showbox#add_label s)
      comp_list ;
    showbox#add_label [||]

(** boilerplate *)


let main options =
  let waiter, wakener = wait () in

  let root = new LTerm_widget.vbox in
  let comp = new frame in
  let completion_box = new completion_box options wakener in
  comp#set completion_box ;
  root#add ~expand:false comp ;

  let show_box = new show_box in
  root#add show_box ;

  (* Express the result as an event mapped on the content of the completion box. *)
  E.(keep (map (completion_event options show_box) completion_box#completion_info)) ;

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