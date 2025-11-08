open Types

(* Tokenizer state *)
type tokenizer = {
  input : string;
  mutable pos : int;
  mutable line : int;
  mutable column : int;
}

let create_tokenizer input = { input; pos = 0; line = 1; column = 1 }

let current_char t =
  if t.pos >= String.length t.input then None
  else Some (String.get t.input t.pos)

let peek_char t =
  if t.pos + 1 >= String.length t.input then None
  else Some (String.get t.input (t.pos + 1))

let current_position t = { line = t.line; column = t.column }

(* Advance position. Mutates the tokenizer state. *)
let advance t =
  match current_char t with
  | Some '\n' ->
      t.pos <- t.pos + 1;
      t.line <- t.line + 1;
      t.column <- 1
  | Some _ ->
      t.pos <- t.pos + 1;
      t.column <- t.column + 1
  | None -> ()

(* Skip whitespace, comments, and anything else that doesn't affect execution. *)
let rec skip_ignorable_tokens t =
  let rec skip_until_newline () =
    match current_char t with
    | Some '\n' -> advance t
    | _ ->
        advance t;
        skip_until_newline ()
  in

  match current_char t with
  | Some (' ' | '\t' | '\n' | '\r') ->
      advance t;
      skip_ignorable_tokens t
  | Some '#' ->
      skip_until_newline ();
      skip_ignorable_tokens t
  | _ -> ()

let read_number t =
  let start_pos = t.pos in
  let has_dot = ref false in

  let rec read_digits () =
    match current_char t with
    | Some '0' .. '9' ->
        advance t;
        read_digits ()
    (* We ignore underscores in numbers, so we can write 1_000_000 and easily
     * understand what it is *)
    | Some '_' ->
        advance t;
        read_digits ()
    (*
     * 1.10 should tokenize a number as expected
     * 1.12.0 should not tokenize, because it's malformed
     * 1..10 should tokenize the `1`, and then bail out, because the dot is for
             a range operator
     *)
    | Some '.' when not !has_dot -> (
        match peek_char t with
        | Some '.' -> ()
        | Some n when n < '0' || n > '9' -> ()
        | _ ->
            has_dot := true;
            advance t;
            read_digits ())
    | _ -> ()
  in

  read_digits ();
  let num_str = String.sub t.input start_pos (t.pos - start_pos) in
  try float_of_string num_str
  with Failure _ ->
    raise (Tokenize_error ("Invalid number: " ^ num_str, current_position t))

let read_identifier t =
  let start_pos = t.pos in

  let rec read_chars () =
    match current_char t with
    | Some ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_') ->
        advance t;
        read_chars ()
    | _ -> ()
  in

  read_chars ();
  String.sub t.input start_pos (t.pos - start_pos)

let read_string t =
  let start_pos = t.pos in

  advance t;

  (*
   * TODO:
   *   1. Escape codes, \t \n etc.
   *   2. String interpolation
   *)
  let rec read_string_content () =
    match current_char t with
    | Some '"' -> advance t
    | _ ->
        advance t;
        read_string_content ()
  in
  read_string_content ();
  String.sub t.input (start_pos + 1) (t.pos - start_pos - 2)

let next_token t =
  skip_ignorable_tokens t;

  let pos = current_position t in

  match current_char t with
  | None -> { kind = EOF; pos }
  | Some c ->
      let kind =
        match c with
        | '0' .. '9' -> Number (read_number t)
        | 'a' .. 'z' | 'A' .. 'Z' | '_' -> (
            let id = read_identifier t in
            match id with
            | "true" -> Bool true
            | "false" -> Bool false
            | "and" -> And
            | "or" -> Or
            | "not" -> Not
            | "nil" -> Nil
            | "if" -> If
            | "else" -> Else
            | "say" -> Say
            | "let" -> Let
            | "in" -> In
            | "def" -> Def
            | "defn" -> Defn
            | "orelse" -> OrElse
            | "mod" -> Mod
            | "div" -> Div
            | "pow" -> Pow
            | s -> Ident s)
        | '+' -> (
            advance t;
            match current_char t with
            | Some '+' ->
                advance t;
                PlusPlus
            | _ -> Plus)
        | '-' -> (
            advance t;
            match current_char t with
            | Some '-' ->
                advance t;
                MinusMinus
            | _ -> Minus)
        | '*' -> (
            advance t;
            match current_char t with
            | Some '*' ->
                advance t;
                StarStar
            | _ -> Star)
        | '/' -> (
            advance t;
            match current_char t with
            | Some '/' ->
                advance t;
                SlashSlash
            | Some '?' ->
                advance t;
                SlashQuestion
            | _ -> Slash)
        | '<' -> (
            advance t;
            match current_char t with
            | Some '=' -> (
                advance t;
                match current_char t with
                | Some '>' ->
                    advance t;
                    LessEqualGreater
                | _ -> LessEqual)
            | _ -> Less)
        | '>' -> (
            advance t;
            match current_char t with
            | Some '=' ->
                advance t;
                GreaterEqual
            | _ -> Greater)
        | '=' -> (
            advance t;
            match current_char t with
            | Some '=' ->
                advance t;
                Equal
            | Some '>' ->
                advance t;
                FatComma
            | _ -> EqualSign)
        | '!' -> (
            advance t;
            match current_char t with
            | Some '=' ->
                advance t;
                NotEqual
            | _ ->
                raise
                  (Tokenize_error
                     ("Unexpected '!' (use 'not' for logical negation)", pos)))
        | '.' -> (
            advance t;
            match current_char t with
            | Some '.' -> (
                advance t;
                match current_char t with
                | Some '<' ->
                    advance t;
                    DotDotLess
                | _ -> DotDot)
            | _ -> Dot)
        | '(' ->
            advance t;
            LParen
        | ')' ->
            advance t;
            RParen
        | '[' ->
            advance t;
            LSquareBracket
        | ']' ->
            advance t;
            RSquareBracket
        | ',' ->
            advance t;
            Comma
        | '\\' ->
            advance t;
            Backslash
        | '"' -> Str (read_string t)
        | ':' ->
            advance t;
            Colon
        | _ ->
            raise
              (Tokenize_error
                 (Printf.sprintf "Unexpected character: '%c'" c, pos))
      in
      { kind; pos }

(* Tokenize entire input into an array *)
let tokenize input =
  let t = create_tokenizer input in
  let rec collect_tokens acc =
    match next_token t with
    | { kind = EOF; _ } as token -> List.rev (token :: acc)
    | token -> collect_tokens (token :: acc)
  in
  Array.of_list (collect_tokens [])
