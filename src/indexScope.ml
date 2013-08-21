open Approx_lexer

module Stream = struct
  type stream =
    { nstream: Nstream.t; last: token; before_last: token; stop: int * int }

  let of_channel chan stop = { nstream = Nstream.of_channel chan;
                               last = COMMENT;
                               before_last = COMMENT;
                               stop; }

  let of_string chan = { nstream = Nstream.of_string chan;
                         last = COMMENT;
                         before_last = COMMENT;
                         stop = (max_int, max_int); }
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
      let top_def, aliases, stream =
        match Stream.next stream with
        | EQUAL, stream ->
            let path, stream = parse_path stream in
            path, [], stream
        | _ -> [], [], stream (* todo *)
      in
      let t = if top_def <> [] then push t (Alias (ident, top_def)) else t in
      (Def, aliases) :: t, stream
  | UIDENT _ -> (* Module.( ... ) *)
      let path, stream = parse_path stream0 in
      (match Stream.next_two stream with
       | DOT, LPAREN, stream -> (Paren, [Open path]) :: t, stream
       | _ -> t, stream)
  | _ -> t, stream

let to_point chan =
  let rec parse_all (t,stream) =
    if Stream.previous stream = EOF then t else parse_all (parse t stream)
  in
  let stream, close = match chan with
    | `File (f, stop)  ->
        let ic = open_in f in
        Stream.of_channel ic stop, (fun () -> close_in ic)
    | `String str      -> Stream.of_string str, (fun () -> ())
  in
  let result = parse_all ([Block,[]], stream) in
  close ();
  result

let rec rev_map_acc f acc = function
  | [] -> acc
  | x::r -> rev_map_acc f (f x :: acc) r

let opens t =
  List.fold_left (fun acc (_, ctx) ->
      List.fold_left (fun acc -> function
          | Open s -> s::acc
          | _ -> acc)
        acc ctx)
    [] t

let aliases t =
  List.fold_left (fun acc (_, ctx) ->
      List.fold_left (fun acc -> function
          | Alias (name,modl) -> (name,modl)::acc
          | _ -> acc)
        acc ctx)
    [] t
