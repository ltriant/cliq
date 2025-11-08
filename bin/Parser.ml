open Types

(* Expressions with positions *)
type expr =
  | ENum of float * position
  | EBoolean of bool * position
  | EString of string * position
  | ENil of position
  | EIdentifier of string * position
  | EArray of expr list * position
  | ERange of expr * expr * bool * position (* start, end, inclusive?, pos *)
  | EBinOp of binop * expr * expr * position
  | EUnaryMinus of expr * position
  | EUnaryNot of expr * position
  | ECondition of
      expr
      * expr
      * expr option
      * position (* condition, then branch, else branch, pos *)
  | ESay of expr list * position
  | ELetIn of (token * expr) list * expr * position
  | EFn of token list * expr * position
  | ECall of expr * expr list * position

and binop =
  | Add
  | ArrayAdd
  | Sub
  | ArraySub
  | Mul
  | ArrayMul
  | Div
  | ArrayDiv
  | And
  | Or
  | Lt
  | Le
  | Gt
  | Ge
  | Eq
  | Ne
  | Modulo
  | IntegerDiv
  | Power
  | Spaceship
  | ArrayConcat
  | OrElseNil
  | IsDivisible

(* Pretty print AST *)
let rec string_of_expr = function
  | ENum (n, _) -> Printf.sprintf "%.1g" n
  | EBoolean (b, _) -> string_of_bool b
  | EString (s, _) -> s
  | ENil _ -> "nil"
  | EIdentifier (s, _) -> s
  | EArray (elems, _) ->
      let elem_strs = List.map string_of_expr elems in
      "[" ^ String.concat ", " elem_strs ^ "]"
  | ERange (start, end_expr, inclusive, _) ->
      Printf.sprintf "(%s %s %s)" (string_of_expr start)
        (if inclusive then ".." else "..<")
        (string_of_expr end_expr)
  | EBinOp (op, left, right, _) ->
      let op_str =
        match op with
        | Add -> "+"
        | ArrayAdd -> "++"
        | Sub -> "-"
        | ArraySub -> "--"
        | Mul -> "*"
        | ArrayMul -> "**"
        | Div -> "/"
        | ArrayDiv -> "//."
        | And -> "and"
        | Or -> "or"
        | Lt -> "<"
        | Le -> "<="
        | Gt -> ">"
        | Ge -> ">="
        | Eq -> "=="
        | Ne -> "!="
        | Modulo -> "mod"
        | IntegerDiv -> "div"
        | Power -> "pow"
        | Spaceship -> "<=>"
        | ArrayConcat -> ":"
        | OrElseNil -> "orelse"
        | IsDivisible -> "/?"
      in
      Printf.sprintf "(%s %s %s)" op_str (string_of_expr left)
        (string_of_expr right)
  | EUnaryMinus (e, _) -> Printf.sprintf "(- %s)" (string_of_expr e)
  | EUnaryNot (e, _) -> Printf.sprintf "(not %s)" (string_of_expr e)
  | ECondition (condE, thenE, None, _) ->
      Printf.sprintf "(if (%s) %s)" (string_of_expr condE)
        (string_of_expr thenE)
  | ECondition (condE, thenE, Some elseE, _) ->
      Printf.sprintf "(if (%s) %s %s)" (string_of_expr condE)
        (string_of_expr thenE) (string_of_expr elseE)
  | ESay (exprs, _) ->
      Printf.sprintf "(say %s)"
        (String.concat " " (List.map string_of_expr exprs))
  | ELetIn (bindings, bodyExpr, _) ->
      Printf.sprintf "(let [%s] %s)"
        (String.concat ", "
           (List.map
              (fun (t, b) ->
                string_of_token_kind t.kind ^ " " ^ string_of_expr b)
              bindings))
        (string_of_expr bodyExpr)
  | EFn (params, bodyExpr, _) ->
      Printf.sprintf "fn(%s) => %s"
        (params
        |> List.map (fun t -> string_of_token_kind t.kind)
        |> String.concat ", ")
        (string_of_expr bodyExpr)
  | ECall (funcName, params, _) ->
      Printf.sprintf "%s(%s)" (string_of_expr funcName)
        (params |> List.map (fun e -> string_of_expr e) |> String.concat ", ")

type stmt = SDef of token * expr | SExpr of expr

let string_of_stmt = function
  | SDef (t, e) ->
      Printf.sprintf "def %s = %s"
        (match t.kind with Ident s -> s | _ -> "unknown")
        (string_of_expr e)
  | SExpr e -> string_of_expr e

(* Convert token to string for debugging *)
let string_of_position (pos:position) = Printf.sprintf "%d:%d" pos.line pos.column

(* Parser state *)
type parser = {
  tokens : token array;
  mutable current : int;
  input : string; (* Original input for error display *)
}

(* Get the line from input at given line number *)
let get_line input line_num =
  let lines = String.split_on_char '\n' input in
  try List.nth lines (line_num - 1) with _ -> ""

(* Create error message with context *)
let error_with_context input (pos : position) msg =
  let line_text = get_line input pos.line in
  let pointer = String.make (pos.column - 1) ' ' ^ "^" in
  Printf.sprintf "%s at line %d, column %d:\n%s\n%s\n%s" msg pos.line pos.column
    line_text pointer
    (if line_text = "" then "(end of input)" else "")

(* Create parser *)
let create_parser tokens input = { tokens; current = 0; input }

(* Peek at current token *)
let peek p =
  if p.current >= Array.length p.tokens then
    p.tokens.(Array.length p.tokens - 1) (* Return EOF *)
  else p.tokens.(p.current)

(* Consume current token and advance *)
let consume p =
  let token = peek p in
  if p.current < Array.length p.tokens then p.current <- p.current + 1;
  token

(* Check if current token matches expected kind *)
let expect p expected =
  let token = peek p in
  if token.kind = expected then consume p
  else
    let msg =
      Printf.sprintf "Expected %s but found %s"
        (string_of_token_kind expected)
        (string_of_token_kind token.kind)
    in
    raise (Parse_error (error_with_context p.input token.pos msg, token.pos))

(* Recursive descent parser following this grammar:
   stmt     -> def_stmt | defn_stmt | expr_stmt
   expr_stmt -> expr
   expr     -> let_in_expr
   let_in_expr -> 'let' IDENT '=' if_expr 'in' say_expr
   say_expr -> 'say' if_expr(, if_expr)*
   if_expr  -> 'if' '(' or_expr ')' expr ('else' expr)?
   or_expr  -> and_expr ('or' and_expr)*
   and_expr -> comp_expr ('and' comp_expr)*
   comp_expr -> range_expr (('<' | '<=' | '>' | '>=' | '==' | '!=') range_expr)?
   range_expr -> arith_expr (('..' | '..<') arith_expr)?
   arith_expr -> term (('+' | '-') term)*
   term     -> factor (('*' | '/') factor)*
   factor   -> NUMBER | BOOL | ARRAY | 'nil' | 'not' factor | '-' factor | '(' expr ')'
   array    -> '[' (expr (',' expr)* )? ']'
*)

let rec parse_stmt p =
  let stmtToken = peek p in
  match stmtToken.kind with
  | Defn -> (
      let _ = consume p in
      (* defn add(a, b) = a + b *)
      let identToken = peek p in
      match identToken.kind with
      | Ident _ ->
          let _ = consume p in
          let _ = expect p LParen in

          let rec parse_params acc =
            let token = peek p in
            match token.kind with
            | Ident _ ->
                let _ = consume p in
                let commaToken = peek p in
                if commaToken.kind = Comma then
                  let _ = consume p in
                  parse_params (token :: acc)
                else List.rev (token :: acc)
            | RParen -> List.rev acc
            | _ ->
                let msg =
                  Printf.sprintf
                    "Expected identifier for function parameter, found %s"
                    (string_of_token_kind token.kind)
                in
                raise
                  (Parse_error
                     (error_with_context p.input token.pos msg, token.pos))
          in

          let params = parse_params [] in
          let _ = expect p RParen in
          let _ = expect p EqualSign in
          let funcBody = parse_expr p in
          SDef (identToken, EFn (params, funcBody, stmtToken.pos))
      | _ ->
          let msg =
            Printf.sprintf "Expected identifier for function name, found %s"
              (string_of_token_kind identToken.kind)
          in
          raise
            (Parse_error
               (error_with_context p.input identToken.pos msg, identToken.pos)))
  | Def -> (
      (* def x = 1234 *)
      let _ = consume p in
      let identToken = peek p in
      match identToken.kind with
      | Ident _ ->
          let _ = consume p in
          let _ = expect p EqualSign in
          let defExpr = parse_expr p in
          SDef (identToken, defExpr)
      | _ ->
          let msg =
            Printf.sprintf "Expected identifier for def, found %s"
              (string_of_token_kind identToken.kind)
          in
          raise
            (Parse_error
               (error_with_context p.input identToken.pos msg, identToken.pos)))
  | _ -> SExpr (parse_expr p)

and parse_expr p = parse_let_expr p

and parse_let_expr p =
  let letToken = peek p in
  match letToken.kind with
  | Let ->
      (* let x = 10, y = 20 in x + y *)
      let _ = consume p in
      let rec parse_var_decls acc =
        let identToken = peek p in
        match identToken.kind with
        | Ident _ -> (
            let _ = consume p in
            let _ = expect p EqualSign in
            let expr = parse_expr p in
            let token = peek p in
            match token.kind with
            | Comma ->
                let _ = consume p in
                parse_var_decls ((identToken, expr) :: acc)
            | _ -> List.rev ((identToken, expr) :: acc))
        | _ -> List.rev acc
      in

      let var_decls = parse_var_decls [] in
      let _ = expect p In in
      let expr = parse_expr p in
      ELetIn (var_decls, expr, letToken.pos)
  | _ -> parse_say_expr p

and parse_say_expr p =
  let sayToken = peek p in
  match sayToken.kind with
  | Say ->
      (* say "1 + 1 = ", 1 + 1 *)
      let _ = consume p in
      let first = parse_expr p in
      let rec parse_say_exprs acc =
        let token = peek p in
        match token.kind with
        | Comma ->
            let _ = consume p in
            let expr = parse_expr p in
            parse_say_exprs (expr :: acc)
        | _ -> List.rev acc
      in
      ESay (parse_say_exprs [ first ], sayToken.pos)
  | _ -> parse_fun_expr p

and parse_fun_expr p =
  let funToken = peek p in
  match funToken.kind with
  | Backslash ->
      (* \x => x + 1 *)
      let _ = consume p in
      let rec parse_params acc =
        let token = peek p in
        match token.kind with
        | Ident _ ->
            let _ = consume p in
            let commaToken = peek p in
            if commaToken.kind = Comma then
              let _ = consume p in
              parse_params (token :: acc)
            else List.rev (token :: acc)
        | _ ->
            let msg =
              Printf.sprintf
                "Expected identifier for function parameter, found %s"
                (string_of_token_kind token.kind)
            in
            raise
              (Parse_error (error_with_context p.input token.pos msg, token.pos))
      in
      let params = parse_params [] in
      let _ = expect p FatComma in
      let funcBody = parse_expr p in
      EFn (params, funcBody, funToken.pos)
  | _ -> parse_if_expr p

and parse_if_expr p =
  let ifToken = peek p in
  match ifToken.kind with
  | If -> (
      let _ = consume p in
      let _ = expect p LParen in
      let ifCond = parse_expr p in
      let _ = expect p RParen in
      let thenExpr = parse_expr p in
      let token = peek p in
      match token.kind with
      | Else ->
          let _ = consume p in
          let elseExpr = parse_expr p in
          ECondition (ifCond, thenExpr, Some elseExpr, ifToken.pos)
      | _ -> ECondition (ifCond, thenExpr, None, ifToken.pos))
  | _ -> parse_or_expr p

and parse_or_expr p =
  let left = parse_and_expr p in
  parse_or_expr_rest p left

and parse_or_expr_rest p left =
  let token = peek p in
  match token.kind with
  | Or ->
      let _ = consume p in
      let right = parse_and_expr p in
      let node = EBinOp (Or, left, right, token.pos) in
      parse_or_expr_rest p node
  | _ -> left

and parse_and_expr p =
  let left = parse_equality_expr p in
  parse_and_expr_rest p left

and parse_and_expr_rest p left =
  let token = peek p in
  match token.kind with
  | And ->
      let _ = consume p in
      let right = parse_equality_expr p in
      let node = EBinOp (And, left, right, token.pos) in
      parse_and_expr_rest p node
  | _ -> left

and parse_equality_expr p =
  let left = parse_concat_expr p in
  let token = peek p in
  match token.kind with
  | Equal ->
      let _ = consume p in
      let right = parse_concat_expr p in
      EBinOp (Eq, left, right, token.pos)
  | NotEqual ->
      let _ = consume p in
      let right = parse_concat_expr p in
      EBinOp (Ne, left, right, token.pos)
  | _ -> left

and parse_concat_expr p =
  let left = parse_comp_expr p in
  parse_concat_expr_rest p left

and parse_concat_expr_rest p left =
  let token = peek p in
  match token.kind with
  | Colon ->
      let _ = consume p in
      let right = parse_comp_expr p in
      let node = EBinOp (ArrayConcat, left, right, token.pos) in
      parse_concat_expr_rest p node
  | _ -> left

and parse_comp_expr p =
  let left = parse_range_expr p in
  let token = peek p in
  match token.kind with
  | Less ->
      let _ = consume p in
      let right = parse_range_expr p in
      EBinOp (Lt, left, right, token.pos)
  | LessEqual ->
      let _ = consume p in
      let right = parse_range_expr p in
      EBinOp (Le, left, right, token.pos)
  | Greater ->
      let _ = consume p in
      let right = parse_range_expr p in
      EBinOp (Gt, left, right, token.pos)
  | GreaterEqual ->
      let _ = consume p in
      let right = parse_range_expr p in
      EBinOp (Ge, left, right, token.pos)
  | LessEqualGreater ->
      let _ = consume p in
      let right = parse_range_expr p in
      EBinOp (Spaceship, left, right, token.pos)
  | _ -> left

and parse_range_expr p =
  let left = parse_arith_expr p in
  let token = peek p in
  match token.kind with
  | DotDot ->
      let _ = consume p in
      let right = parse_arith_expr p in
      ERange (left, right, true, token.pos)
  | DotDotLess ->
      let _ = consume p in
      let right = parse_arith_expr p in
      ERange (left, right, false, token.pos)
  | _ -> left

and parse_arith_expr p =
  let left = parse_term p in
  parse_arith_expr_rest p left

and parse_arith_expr_rest p left =
  let token = peek p in
  match token.kind with
  | Plus ->
      let _ = consume p in
      let right = parse_term p in
      let node = EBinOp (Add, left, right, token.pos) in
      parse_arith_expr_rest p node
  | PlusPlus ->
      let _ = consume p in
      let right = parse_term p in
      let node = EBinOp (ArrayAdd, left, right, token.pos) in
      parse_arith_expr_rest p node
  | Minus ->
      let _ = consume p in
      let right = parse_term p in
      let node = EBinOp (Sub, left, right, token.pos) in
      parse_arith_expr_rest p node
  | MinusMinus ->
      let _ = consume p in
      let right = parse_term p in
      let node = EBinOp (ArraySub, left, right, token.pos) in
      parse_arith_expr_rest p node
  | _ -> left

and parse_term p =
  let left = parse_orelse p in
  parse_term_rest p left

and parse_term_rest p left =
  let token = peek p in
  match token.kind with
  | Star ->
      let _ = consume p in
      let right = parse_orelse p in
      let node = EBinOp (Mul, left, right, token.pos) in
      parse_term_rest p node
  | StarStar ->
      let _ = consume p in
      let right = parse_orelse p in
      let node = EBinOp (ArrayMul, left, right, token.pos) in
      parse_term_rest p node
  | Slash ->
      let _ = consume p in
      let right = parse_orelse p in
      let node = EBinOp (Div, left, right, token.pos) in
      parse_term_rest p node
  | SlashSlash ->
      let _ = consume p in
      let right = parse_orelse p in
      let node = EBinOp (ArrayDiv, left, right, token.pos) in
      parse_term_rest p node
  | SlashQuestion ->
      let _ = consume p in
      let right = parse_orelse p in
      let node = EBinOp (IsDivisible, left, right, token.pos) in
      parse_term_rest p node
  | Mod ->
      let _ = consume p in
      let right = parse_orelse p in
      let node = EBinOp (Modulo, left, right, token.pos) in
      parse_term_rest p node
  | Div ->
      let _ = consume p in
      let right = parse_orelse p in
      let node = EBinOp (IntegerDiv, left, right, token.pos) in
      parse_term_rest p node
  | Pow ->
      let _ = consume p in
      let right = parse_orelse p in
      let node = EBinOp (Power, left, right, token.pos) in
      parse_term_rest p node
  | _ -> left

and parse_orelse p =
  let left = parse_call p in
  parse_orelse_rest p left

and parse_orelse_rest p left =
  let token = peek p in
  match token.kind with
  | OrElse ->
      let _ = consume p in
      let right = parse_call p in
      let node = EBinOp (OrElseNil, left, right, token.pos) in
      parse_orelse_rest p node
  | _ -> left

and parse_call p =
  let left = parse_primary p in
  parse_call_rest p left

and parse_call_rest p left =
  let token = peek p in
  match token.kind with
  | LParen ->
      (* function(arg1, arg2) *)
      let _ = consume p in
      let rec parse_args acc =
        let token = peek p in
        match token.kind with
        | RParen ->
            let _ = consume p in
            List.rev acc
        | _ ->
            let expr = parse_expr p in
            let maybeComma = peek p in
            if maybeComma.kind == Comma then
              let _ = consume p in
              parse_args (expr :: acc)
            else
              let _ = expect p RParen in
              List.rev (expr :: acc)
      in
      let params = parse_args [] in
      parse_call_rest p (ECall (left, params, token.pos))
  | Dot ->
      (* foo.function(arg1, arg2) *)
      let _ = consume p in
      let funcName = parse_primary p in
      let _ = expect p LParen in
      let rec parse_args acc =
        let token = peek p in
        match token.kind with
        | RParen ->
            let _ = consume p in
            List.rev acc
        | _ ->
            let expr = parse_expr p in
            let maybeComma = peek p in
            if maybeComma.kind == Comma then
              let _ = consume p in
              parse_args (expr :: acc)
            else
              let _ = expect p RParen in
              List.rev (expr :: acc)
      in
      let params = parse_args [ left ] in
      parse_call_rest p (ECall (funcName, params, token.pos))
  | _ -> left

and parse_primary p =
  let token = peek p in
  match token.kind with
  | Number n ->
      let _ = consume p in
      ENum (n, token.pos)
  | Bool b ->
      let _ = consume p in
      EBoolean (b, token.pos)
  | Str s ->
      let _ = consume p in
      EString (s, token.pos)
  | Nil ->
      let _ = consume p in
      ENil token.pos
  | Ident s ->
      let _ = consume p in
      EIdentifier (s, token.pos)
  | LSquareBracket -> parse_array p
  | Not ->
      let _ = consume p in
      let operand = parse_primary p in
      EUnaryNot (operand, token.pos)
  | Minus ->
      let _ = consume p in
      let operand = parse_primary p in
      EUnaryMinus (operand, token.pos)
  | LParen ->
      let _ = consume p in
      let expr = parse_expr p in
      let _ = expect p RParen in
      expr
  | _ ->
      let msg =
        Printf.sprintf "Unexpected token: %s" (string_of_token_kind token.kind)
      in
      raise (Parse_error (error_with_context p.input token.pos msg, token.pos))

and parse_array p =
  let start_token = peek p in
  let _ = expect p LSquareBracket in

  let token = peek p in
  if token.kind = RSquareBracket then
    let _ = consume p in
    EArray ([], start_token.pos)
  else
    let first = parse_expr p in
    let rec parse_elements acc =
      let token = peek p in
      match token.kind with
      | Comma ->
          let _ = consume p in
          let elem = parse_expr p in
          parse_elements (elem :: acc)
      | RSquareBracket ->
          let _ = consume p in
          List.rev acc
      | _ ->
          let msg =
            Printf.sprintf "Expected ',' or ']' but found %s"
              (string_of_token_kind token.kind)
          in
          raise
            (Parse_error (error_with_context p.input token.pos msg, token.pos))
    in
    let elements = parse_elements [ first ] in
    EArray (elements, start_token.pos)
