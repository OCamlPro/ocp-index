open Lwt
open Lwt_react
open LTerm_widget
open CamomileLibraryDyn.Camomile

exception Exit

let colorise opts =
  LibIndex.Format.no_color

let format_answer opts id =
  let colorise = colorise opts in

  let buf = Buffer.create 5 in
  let fmt = Format.formatter_of_buffer buf in
  let print = Format.fprintf fmt in

  print "@[<hv 4>" ;
  LibIndex.Format.kind ~colorise fmt id;
  print " ";
  LibIndex.Format.path ~colorise fmt id;
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
  print "@]@?" ;

  Buffer.contents buf


(** Widgets ! *)

class label s = object (self)
  inherit LTerm_widget.label s

  val mutable style = LTerm_style.none
  method! update_resources =
    style <- LTerm_resources.get_style self#resource_class self#resources

  method! draw ctx focused =
    LTerm_draw.fill_style ctx style;
    LTerm_draw.draw_string_aligned ctx 0 LTerm_geom.H_align_left self#text

end

class completion_box = object (self)
  inherit LTerm_edit.edit ()

  val size_request = { LTerm_geom. rows = 1; cols = 1 }
  method! size_request = size_request

  method content =
    let open Zed_edit in
    let ev = changes self#engine in
    E.map (fun _ -> self#text) ev

end

class show_box = object (self)
  inherit LTerm_widget.vbox

  method add_label s =
    let l = new label s in
    self#add ~expand:false l

  method clean =
    List.iter self#remove self#children

end

(** Events *)

let handle_key options wake =
  let open LTerm_key in function
    | { code = Escape } -> wakeup wake () ; true
    | { control = true ; meta = false; shift = false ; code = LTerm_key.Char ch }
      when ch = UChar.of_char 'c'->
        wakeup wake () ; true
    | _ -> false


let handle_event options wake = function
  | LTerm_event.Key key ->
      handle_key options wake key
  | _ -> false


let completion_event options showbox comp_ev =
  showbox#clean ;
  let response =
    LibIndex.complete
      options.IndexOptions.lib_info
      ~filter:(IndexOptions.filter options)
      comp_ev
  in
  Printf.fprintf stderr "ping %d\n" (List.length response);
  List.iter
    (fun id ->
       let s = format_answer options id in
       Printf.fprintf stderr "%s\n" s ;
       showbox#add_label s)
    response ;
  showbox#add_label ""

(** bolterplate *)


let main options =
  let waiter, wakener = wait () in

  let root = new LTerm_widget.vbox in
  let comp = new frame in
  let completion_box = new completion_box in
  comp#set completion_box ;
  root#add ~expand:false comp ;

  let show_box = new show_box in
  root#add show_box ;

  root#on_event (handle_event options wakener) ;

  (* Express the result as an event mapped on the content of the completion box. *)
  E.(keep (map (completion_event options show_box) completion_box#content)) ;

  (* We don't want any newlines in the completion bar. *)
  LTerm_edit.unbind LTerm_key.(
      [{ control = false; meta = false; shift = false; code = Enter }]
    ) ;

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
