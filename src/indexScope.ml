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

(* - Input stream handling - *)

open Approx_lexer

module Stream = struct
  type stream =
    { nstream: Nstream.t; last: token; before_last: token; stop: int * int }

  let of_nstream nstream stop = {
    nstream;
    last = COMMENT;
    before_last = COMMENT;
    stop;
  }

  let next stream =
    let shift stream tok =
      { stream with
        last = tok;
        before_last = match stream.last with
          | COMMENT -> stream.before_last
          | tok -> tok }
    in
    match Nstream.next stream.nstream with
    | Some ({Nstream.token; region}, nstream) ->
        let pos = Pos.Region.snd region in
        let line, col = stream.stop in
        if Lexing.(pos.pos_lnum < line
                   || pos.pos_lnum = line && pos.pos_cnum - pos.pos_bol < col)
        then token, shift {stream with nstream} token
        else EOF, shift stream EOF
    | _ -> EOF, shift stream EOF

  let next_two stream =
    let tok1, stream = next stream in
    let tok2, stream = next stream in
    tok1, tok2, stream

  let next_three stream =
    let tok1, stream = next stream in
    let tok2, stream = next stream in
    let tok3, stream = next stream in
    tok1, tok2, tok3, stream

  let previous stream = stream.before_last
end

let close_def stream = match Stream.previous stream with
  | AMPERSAND | AMPERAMPER | BARBAR | BEGIN | COLONCOLON | COLONEQUAL | COMMA
  | DO | DOWNTO | ELSE | EQUAL | GREATER | IF | IN | INFIXOP0 _ | INFIXOP1 _
  | INFIXOP2 _ | INFIXOP3 _ | INFIXOP4 _ | LBRACE | LBRACELESS | LBRACKET
  | LBRACKETBAR | LBRACKETLESS | LBRACKETGREATER | LESS | LESSMINUS | LPAREN
  | MATCH | MINUS | MINUSDOT | MINUSGREATER | OR | PLUS | PLUSDOT | QUESTION
  | QUESTIONQUESTION | SEMI | STAR | THEN | TO | TRY | WHEN | WHILE | TILDE ->
      false
  | _ -> true

let parse_path stream =
  let rec aux acc stream =
    match Stream.next_two stream with
    | DOT, UIDENT i, stream -> aux (i::acc) stream
    | _ -> List.rev acc, stream
  in
  match Stream.next stream with
  | UIDENT i, stream ->
      let path, stream = aux [] stream in
      i::path, stream
  | _ -> [], stream

let rec skip_to_next_paren stream =
  let tok, stream = Stream.next stream in
  match tok with
  | RPAREN | EOF -> stream
  | _      -> skip_to_next_paren stream

(* - Now for the interesting stuff - *)

type scope = Def | Block | Paren | Brace

type env = Alias of string * string list | Open of string list

type t = (scope * env list) list

let rec close t scope = match t with
  | [] -> []
  | (scope1,_)::r when scope1 = scope -> r
  | _::r -> close r scope

let maybe_close t scope = match t with
  | (scope1,_)::r when scope1 = scope -> r
  | t -> t

let push t info = match t with
  | (scope, infos)::r -> (scope, info::infos) :: r
  | [] -> [Block, [info]] (* print error ? *)

(* [ (X : S) ]* *)
let parse_functor_args stream =
  let rec aux stream =
    let parse_error = [], stream in
    match Stream.next_three stream with
    | LPAREN, UIDENT x, COLON, stream ->
        begin match parse_path stream with
          | [], _       -> parse_error
          | s , stream ->
              (* XXX: convert module constraints into module aliases ?*)
              let stream = skip_to_next_paren stream in
              let args, stream = aux stream in
              Alias (x,s) :: args, stream
        end
    | _ -> parse_error in
  aux stream

(* functor (X: S) -> *)
let parse_functor stream =
  let rec aux stream =
    let parse_error = [], stream in
    match Stream.next stream with
    | FUNCTOR, stream ->
        let args, stream = parse_functor_args stream in
        begin match Stream.next stream with
          | MINUSGREATER, stream ->
              let rest, stream = aux stream in
              args @ rest, stream
          | _ -> parse_error
        end
    | _ -> parse_error in
  aux stream

let parse t stream0 =
  let tok, stream = Stream.next stream0 in
  match tok with
  | STRUCT | SIG | BEGIN | OBJECT -> (Block, []) :: t, stream
  | END -> close t Block, stream
  | LPAREN -> (Paren, []) :: t, stream
  | RPAREN -> close t Paren, stream
  | LBRACE -> (Brace, []) :: t, stream
  | RBRACE -> close t Brace, stream
  | OPEN ->
      let t = if Stream.previous stream = LET then t else maybe_close t Def in
      let path, stream = parse_path stream in
      push t (Open path), stream
  | INCLUDE ->
      let path, stream = parse_path stream in
      push t (Open path), stream
  | LET when close_def stream -> (Def, []) :: maybe_close t Def, stream
  | MODULE ->
      let t = if close_def stream then maybe_close t Def else t in
      let ident, stream = match Stream.next stream with
        | UIDENT u, stream -> u, stream
        | TYPE, stream1 -> (match Stream.next stream1 with
            | UIDENT u, stream -> u, stream
            | _ -> "", stream)
        | _ -> "", stream in
      let functor_pre_args, stream = parse_functor_args stream in
      let top_def, stream =
        match Stream.next stream with
        | EQUAL, stream1 ->
            begin match parse_path stream1 with
              | []  , _      -> [],   stream
              | path, stream -> path, stream
            end
        | _ -> [], stream (* todo *)
      in
      let functor_post_args, stream =
        match Stream.next stream with
        | EQUAL, stream -> parse_functor stream
        | _ -> [], stream in
      let aliases = functor_pre_args @ functor_post_args in
      let t = if top_def <> [] then push t (Alias (ident, top_def)) else t in
      (Def, Open [ident] :: aliases) :: t, stream
  | UIDENT _ -> (* Module.( ... ) *)
      let path, stream = parse_path stream0 in
      (match Stream.next_two stream with
       | DOT, LPAREN, stream -> (Paren, [Open path]) :: t, stream
       | _ -> t, stream)
  | _ -> t, stream

let read_aux ?(line=max_int) ?(column=max_int) nstream =
  let rec parse_all (t,stream) =
    if Stream.previous stream = EOF then t else parse_all (parse t stream)
  in
  parse_all ([Block,[]], Stream.of_nstream nstream (line, column))

let read ?line ?column chan =
  read_aux ?line ?column (Nstream.of_channel chan)

let read_string string =
  read_aux (Nstream.of_string string)

let to_list t =
  List.fold_left (fun acc (_, ctx) -> List.rev_append ctx acc) [] t
