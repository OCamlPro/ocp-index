type scope = Let | Begin | Paren | Brace | Struct | Sig | Object

type env = Alias of string * string | Open of string

type t = (scope * env list) list

let rec close t f = match t with
  | [] -> []
  | (scope,_)::r when f scope -> r
  | _::r -> close r f

let push (scope, infos) info = (scope, info::infos)

open Approx_token

module Stream = struct
  type stream = { nstream: Nstream.t; last: token; before_last: token }

  let next stream =
    let shift stream tok =
      { stream with
        last = tok;
        before_last = match stream.last with
          | COMMENT _ -> stream.before_last
          | tok -> tok }
    in
    match Nstream.next stream.nstream with
    | None -> EOF, shift stream EOF
    | Some ({token}, stream) -> token, shift stream token

  let previous stream = stream.before_last
end

let close_let stream = match Stream.previous stream with
  | AMPERSAND | AMPERAMPER | BARBAR | BEGIN | COLONCOLON | COLONEQUAL
  | COMMA | DO | DOWNTO | ELSE | EQUAL | GREATER | IF | IN
  | INFIXOP0 _ | INFIXOP1 _ | INFIXOP2 _ | INFIXOP3 _ | INFIXOP4 _
  | LBRACE | LBRACELESS
  | LBRACKET | LBRACKETBAR | LBRACKETLESS | LBRACKETGREATER
  | LESS | LESSMINUS | LPAREN | MATCH | MINUS | MINUSDOT | MINUSGREATER | OR
  | PLUS | PLUSDOT | QUESTION | QUESTIONQUESTION | SEMI | STAR | THEN
  | TO | TRY | WHEN | WHILE
  | TILDE -> false
  | _ -> true

let parse_path stream =
  let rec aux acc stream =
    match Stream.next stream with
    | UIDENT i, stream ->
        (match Stream.next stream with
         | DOT, stream -> aux (i::acc) stream
         | _ -> List.rev (i::acc), stream)
    | _ -> List.rev acc, stream
  in
  String.concat "." i (aux stream)

let parse t stream =
  let tok, stream = Stream.next stream in
  match tok with
  | STRUCT -> (Struct, []) :: t, stream
  | LET when close_let stream -> (Let, []) :: t, stream
  | LPAREN -> (Struct, []) :: t, stream
  | BEGIN -> (Begin, []) :: t, stream
  | LBRACE -> (Brace, []) :: t, stream
  | OBJECT -> (Object, []) :: t, stream
  | SIG -> (Sig, []) :: t, stream
  | END ->
      close t (function
        | Struct | Object | Begin -> true
        | _ -> false),
      stream
  | RPAREN -> close t ((=) Paren), stream
  | RBRACE -> close t ((=) Brace), stream
  | OPEN ->
      let path, stream = parse_path stream in
      push t (Open path), stream
  | MODULE 
