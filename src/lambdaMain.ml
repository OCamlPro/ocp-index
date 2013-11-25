open Lwt
open Lwt_react
open LTerm_widget
open CamomileLibraryDyn.Camomile

exception Exit


type state = {
  query : string ;
  scroll: int; (* for scrolling: how many lines to skip before printing *)
  results: string list;
  (* for completion: what we are completing on, and
     the index of the next completion item to select *)
}

let state, update_state =
  S.create { query = ""; scroll = 0; results = [] }

(** Widgets ! *)

class completion_box = object
  inherit LTerm_edit.edit ()

  val size_request = { LTerm_geom. rows = 1; cols = 1 }
  method! size_request = size_request

end

(** Events *)

let handle_key wake =
  let open LTerm_key in function
    | { code = Escape } -> wakeup wake () ; true
    | { control = true ; meta = false; shift = false ; code = LTerm_key.Char ch }
      when ch = UChar.of_char 'c'->
        wakeup wake () ; true
    | { code = Enter } -> false
    | _ -> false

let handle_event wake = function
  | LTerm_event.Key key ->
      handle_key wake key
  | _ -> false


(** bolterplate *)


let main options =
  let waiter, wakener = wait () in

  let root = new LTerm_widget.vbox in
  let comp = new frame in
  comp#set (new completion_box) ;
  root#add ~expand:false comp ;

  let show_box = new LTerm_widget.vbox in
  root#add show_box ;

  root#on_event (handle_event wakener) ;

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
