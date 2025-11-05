(* Position information for tokens *)
type position = { line : int; column : int }

(* Token type with position information *)
type token = { kind : token_kind; pos : position }

and token_kind =
  | Number of float
  | Bool of bool
  | Str of string
  | Ident of string
  | Plus
  | Minus
  | Star
  | Slash
  | And
  | Or
  | Not
  | Less
  | LessEqual
  | Greater
  | GreaterEqual
  | Equal
  | NotEqual
  | LParen
  | RParen
  | LSquareBracket
  | RSquareBracket
  | Comma
  | Dot
  | DotDot
  | DotDotLess
  | Nil
  | If
  | Else
  | Say
  | Let
  | In
  | EqualSign
  | Def
  | Defn
  | Backslash
  | FatComma
  | OrElse
  | Mod
  | Div
  | LessEqualGreater
  | Colon
  | EOF

let string_of_token_kind = function
  | Number n -> Printf.sprintf "Number(%g)" n
  | Bool b -> Printf.sprintf "Bool(%b)" b
  | Str s -> Printf.sprintf "String(%s)" s
  | Nil -> "ENil"
  | Ident i -> i
  | Plus -> "+"
  | Minus -> "-"
  | Star -> "*"
  | Slash -> "/"
  | And -> "and"
  | Or -> "or"
  | Not -> "not"
  | Less -> "<"
  | LessEqual -> "<="
  | Greater -> ">"
  | GreaterEqual -> ">="
  | Equal -> "=="
  | NotEqual -> "!="
  | LParen -> "("
  | RParen -> ")"
  | LSquareBracket -> "["
  | RSquareBracket -> "]"
  | Comma -> ","
  | Dot -> "."
  | DotDot -> ".."
  | DotDotLess -> "..<"
  | If -> "if"
  | Else -> "else"
  | Say -> "say"
  | Let -> "let"
  | In -> "in"
  | EqualSign -> "="
  | Def -> "def"
  | Defn -> "defn"
  | Backslash -> "\\"
  | FatComma -> "=>"
  | OrElse -> "orelse"
  | Mod -> "mod"
  | Div -> "div"
  | LessEqualGreater -> "<=>"
  | Colon -> ":"
  | EOF -> "EOF"

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
  | Sub
  | Mul
  | Div
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
  | Spaceship
  | ArrayConcat
  | OrElseNil

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
        | Sub -> "-"
        | Mul -> "*"
        | Div -> "/"
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
        | Spaceship -> "<=>"
        | ArrayConcat -> ":"
        | OrElseNil -> "orelse"
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

(* Exception for tokenization errors *)
exception Tokenize_error of string * position

(* Exception for parsing errors *)
exception Parse_error of string * position

(* Exception for runtime errors *)
exception Runtime_error of string * position

type value =
  | VNum of float
  | VBool of bool
  | VStr of string
  | VArray of value array
  | VFunction of string * int * (interpreter -> value array -> value)
  | VNil

(* Intepreter environment *)
and environment = {
  values : (string, value) Hashtbl.t;
  mutable nFuncs : int;
  enclosing : environment option;
}

and interpreter = { mutable env : environment }

let type_of_value = function
  | VNum _ -> "num"
  | VBool _ -> "bool"
  | VStr _ -> "str"
  | VArray _ -> "array"
  | VNil -> "nil"
  | VFunction _ -> "fn"

(* Convert value to boolean for logical operations *)
let to_bool v pos =
  match v with
  | VBool b -> b
  | x ->
      raise
        (Runtime_error ("Expected boolean value, got " ^ type_of_value x, pos))

(* Convert value to number for arithmetic operations *)
let to_num v pos =
  match v with
  | VNum n -> n
  | x ->
      raise
        (Runtime_error ("Expected number value, got " ^ type_of_value x, pos))

(* In OCaml, string_of_float 10.0 == "10.", so we chop off the trailing decimal point *)
let cliq_string_of_float f =
  let rv = string_of_float f in
  if String.ends_with rv ~suffix:"." then String.sub rv 0 (String.length rv - 1)
  else rv

(* Arithmetic operations on values *)
let v_plus a b pos =
  match (a, b) with
  | VNum x, VNum y -> VNum (x +. y)
  | VStr x, VStr y -> VStr (x ^ y)
  | VNum x, VStr y -> VStr (cliq_string_of_float x ^ y)
  | VStr x, VNum y -> VStr (x ^ cliq_string_of_float y)
  | VArray xs, VArray ys -> VArray (Array.concat [ xs; ys ])
  | _ ->
      raise
        (Runtime_error
           ( Printf.sprintf "Unexpected `+` operands: %s, %s" (type_of_value a)
               (type_of_value b),
             pos ))

let rec v_eq a b pos =
  match (a, b) with
  | VNil, VNil -> VBool true
  | VNum x, VNum y -> VBool (x = y)
  | VBool x, VBool y -> VBool (x = y)
  | VStr x, VStr y -> VBool (x = y)
  | VStr x, VNum y -> VBool (x = cliq_string_of_float y)
  | VNum x, VStr y -> VBool (cliq_string_of_float x = y)
  | VArray xs, VArray ys ->
      VBool (Array.equal (fun m n -> to_bool (v_eq m n pos) pos) xs ys)
  | _ ->
      raise
        (Runtime_error
           ( Printf.sprintf "Unexpected `==` operands: %s, %s" (type_of_value a)
               (type_of_value b),
             pos ))

let v_ne a b pos =
  try
    let equal = v_eq a b pos in
    match equal with
    | VBool x -> VBool (not x)
    (* If we're not returning a VBool, then we've broken something in the v_eq function *)
    | _ -> raise (Runtime_error ("Unreachable", pos))
  with Runtime_error (_, _) ->
    raise
      (Runtime_error
         ( Printf.sprintf "Unexpected `!=` operands: %s, %s" (type_of_value a)
             (type_of_value b),
           pos ))

let v_not a pos =
  match a with
  | VBool x -> VBool (not x)
  | _ ->
      raise
        (Runtime_error
           (Printf.sprintf "Unexpected `not` operand: %s" (type_of_value a), pos))

let is_truthy v =
  match v with
  | VNum n -> n > 0.0
  | VBool b -> b
  | VStr s -> String.length s > 0
  | VArray a -> Array.length a > 0
  | VNil -> false
  | VFunction _ -> false

(* Convert value to string for printing *)
let rec string_of_value = function
  | VNum n -> Printf.sprintf "%g" n
  | VBool b -> string_of_bool b
  | VStr s -> s
  | VArray elems ->
      let elem_strs = elems |> Array.map string_of_value |> Array.to_list in
      "[" ^ String.concat ", " elem_strs ^ "]"
  | VNil -> "nil"
  | VFunction (n, _, _) -> Printf.sprintf "<fn %s>" n

let create_env enclosing = { values = Hashtbl.create 10; nFuncs = 0; enclosing }
let set_value e var value = Hashtbl.replace e.values var value

let new_interpreter env =
  set_value env "length"
    (VFunction
       ( "length",
         1,
         fun _ args ->
           match args with
           | [| VNil |] -> VNil
           | [| VArray xs |] -> VNum (xs |> Array.length |> float_of_int)
           | [| VStr s |] -> VNum (s |> String.length |> float_of_int)
           | _ ->
               raise
                 (Runtime_error
                    ("length: expected array", { line = 0; column = 0 })) ));

  set_value env "map"
    (VFunction
       ( "map",
         2,
         fun i args ->
           match args with
           | [| VNil |] -> VNil
           | [| VArray xs; VFunction (_, arity, fn) |] ->
               if arity != 1 then
                 raise
                   (Runtime_error
                      ( "map: function should have arity of 1",
                        { line = 0; column = 0 } ))
               else VArray (xs |> Array.map (fun x -> fn i [| x |]))
           | [| VArray _; a |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf "map: expected a function to apply, got %s"
                        (type_of_value a),
                      { line = 0; column = 0 } ))
           | [| a; VFunction _ |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf "map: expected array to map, got %s"
                        (type_of_value a),
                      { line = 0; column = 0 } ))
           | _ ->
               raise
                 (Runtime_error
                    ( "map: expected array, and function",
                      { line = 0; column = 0 } )) ));

  set_value env "filter"
    (VFunction
       ( "filter",
         2,
         fun i args ->
           match args with
           | [| VNil |] -> VNil
           | [| VArray xs; VFunction (_, arity, fn) |] ->
               if arity != 1 then
                 raise
                   (Runtime_error
                      ( "filter: function should have arity of 1",
                        { line = 0; column = 0 } ))
               else
                 VArray
                   (xs |> Array.to_list
                   |> List.filter (fun x -> is_truthy (fn i [| x |]))
                   |> Array.of_list)
           | [| VArray _; a |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf
                        "filter: expected a function to apply, got %s"
                        (type_of_value a),
                      { line = 0; column = 0 } ))
           | [| a; VFunction _ |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf "filter: expected array to filter, got %s"
                        (type_of_value a),
                      { line = 0; column = 0 } ))
           | _ ->
               raise
                 (Runtime_error
                    ( "filter: expected array, and function",
                      { line = 0; column = 0 } )) ));

  set_value env "reduce"
    (VFunction
       ( "reduce",
         3,
         fun i args ->
           match args with
           | [| VNil |] -> VNil
           | [| VArray xs; VFunction (_, arity, fn); init |] ->
               if arity != 2 then
                 raise
                   (Runtime_error
                      ( "reduce: function should have arity of 2",
                        { line = 0; column = 0 } ))
               else
                 xs |> Array.to_list
                 |> List.fold_left (fun acc v -> fn i [| acc; v |]) init
           | [| VArray _; a; _ |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf
                        "reduce: expected a function for reduce, got %s"
                        (type_of_value a),
                      { line = 0; column = 0 } ))
           | [| a; VFunction _; _ |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf
                        "reduce: expected an array to reduce, got %s"
                        (type_of_value a),
                      { line = 0; column = 0 } ))
           | _ ->
               raise
                 (Runtime_error
                    ( "reduce: expected array, function, and initial value",
                      { line = 0; column = 0 } )) ));

  set_value env "zip"
    (VFunction
       ( "zip",
         2,
         fun _ args ->
           match args with
           | [| VNil; _ |] -> VNil
           | [| VArray xs; VArray ys |] ->
               if Array.length xs != Array.length ys then
                 raise
                   (Runtime_error
                      ( Printf.sprintf
                          "zip: expected arrays of same length, got %d, %d"
                          (Array.length xs) (Array.length ys),
                        { line = 0; column = 0 } ))
               else
                 VArray
                   (ys |> Array.to_seq
                   |> Seq.zip (xs |> Array.to_seq)
                   |> Seq.map (fun (a, b) -> VArray [| a; b |])
                   |> Array.of_seq)
           | [| a; b |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf "zip: expected arrays, got %s, %s"
                        (type_of_value a) (type_of_value b),
                      { line = 0; column = 0 } ))
           | _ ->
               raise (Runtime_error ("Unreachable", { line = 0; column = 0 }))
       ));

  set_value env "sum"
    (VFunction
       ( "sum",
         1,
         fun _ args ->
           match args with
           | [| VNil |] -> VNil
           | [| VArray xs |] ->
               VNum
                 (xs |> Array.to_list
                 |> List.fold_left
                      (fun acc v ->
                        match v with
                        | VNum vv -> acc +. vv
                        | e ->
                            raise
                              (Runtime_error
                                 ( Printf.sprintf
                                     "Expected array of numbers, found %s"
                                     (type_of_value e),
                                   { line = 0; column = 0 } )))
                      0.)
           | [| a |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf "sum: expected array, got %s"
                        (type_of_value a),
                      { line = 0; column = 0 } ))
           | _ ->
               raise (Runtime_error ("Unreachable", { line = 0; column = 0 }))
       ));

  set_value env "first"
    (VFunction
       ( "first",
         1,
         fun _ args ->
           match args with
           | [| VNil |] -> VNil
           | [| VArray xs |] -> if Array.length xs > 0 then xs.(0) else VNil
           | [| x |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf "first: expected array, got %s"
                        (type_of_value x),
                      { line = 0; column = 0 } ))
           | _ ->
               raise (Runtime_error ("Unreachable", { line = 0; column = 0 }))
       ));

  set_value env "second"
    (VFunction
       ( "second",
         1,
         fun _ args ->
           match args with
           | [| VNil |] -> VNil
           | [| VArray xs |] -> if Array.length xs > 1 then xs.(1) else VNil
           | [| x |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf "second: expected array, got %s"
                        (type_of_value x),
                      { line = 0; column = 0 } ))
           | _ ->
               raise (Runtime_error ("Unreachable", { line = 0; column = 0 }))
       ));

  set_value env "last"
    (VFunction
       ( "last",
         1,
         fun _ args ->
           match args with
           | [| VNil |] -> VNil
           | [| VArray xs |] ->
               if Array.length xs > 0 then xs.(Array.length xs - 1) else VNil
           | [| x |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf "last: expected array, got %s"
                        (type_of_value x),
                      { line = 0; column = 0 } ))
           | _ ->
               raise (Runtime_error ("Unreachable", { line = 0; column = 0 }))
       ));

  set_value env "rest"
    (VFunction
       ( "rest",
         1,
         fun _ args ->
           match args with
           | [| VNil |] -> VNil
           | [| VArray xs |] ->
               let n_items = Array.length xs in
               if n_items > 0 then VArray (Array.sub xs 1 (n_items - 1))
               else VNil
           | [| x |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf "rest: expected array, got %s"
                        (type_of_value x),
                      { line = 0; column = 0 } ))
           | _ ->
               raise (Runtime_error ("Unreachable", { line = 0; column = 0 }))
       ));

  set_value env "drop"
    (VFunction
       ( "drop",
         2,
         fun _ args ->
           match args with
           | [| VNil; _ |] -> VNil
           | [| VArray xs; VNum n |] ->
               let n_items = Array.length xs in
               let drop_n = int_of_float n in
               VArray (Array.sub xs drop_n (n_items - drop_n))
           | [| x; y |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf
                        "drop: expected array and number, got %s, %s"
                        (type_of_value x) (type_of_value y),
                      { line = 0; column = 0 } ))
           | _ ->
               raise (Runtime_error ("Unreachable", { line = 0; column = 0 }))
       ));

  set_value env "dropLast"
    (VFunction
       ( "dropLast",
         2,
         fun _ args ->
           match args with
           | [| VNil; _ |] -> VNil
           | [| VArray xs; VNum n |] ->
               let n_items = Array.length xs in
               VArray (Array.sub xs 0 (n_items - int_of_float n))
           | [| x; y |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf
                        "dropLast: expected array and number, got %s, %s"
                        (type_of_value x) (type_of_value y),
                      { line = 0; column = 0 } ))
           | _ ->
               raise (Runtime_error ("Unreachable", { line = 0; column = 0 }))
       ));

  set_value env "nth"
    (VFunction
       ( "nth",
         2,
         fun _ args ->
           match args with
           | [| VNil; _ |] -> VNil
           | [| VArray xs; VNum n |] ->
               let n_items = Array.length xs in
               let n = int_of_float n in
               if n < 0 then xs.(n_items - abs n) else xs.(n)
           | [| x; y |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf
                        "drop: expected array and number, got %s, %s"
                        (type_of_value x) (type_of_value y),
                      { line = 0; column = 0 } ))
           | _ ->
               raise (Runtime_error ("Unreachable", { line = 0; column = 0 }))
       ));

  set_value env "sort"
    (VFunction
       ( "sort",
         2,
         fun i args ->
           match args with
           | [| VNil; _ |] -> VNil
           | [| VArray xs; VFunction (_, arity, fn) |] ->
               if arity != 2 then
                 raise
                   (Runtime_error
                      ( "reduce: function should have arity of 2",
                        { line = 0; column = 0 } ))
               else
                 VArray
                   (xs |> Array.to_list
                   |> List.sort (fun a b ->
                       match fn i [| a; b |] with
                       | VNum n -> int_of_float n
                       | a ->
                           raise
                             (Runtime_error
                                ( Printf.sprintf
                                    "Expected sort function to return num, got \
                                     %s"
                                    (type_of_value a),
                                  { line = 0; column = 0 } )))
                   |> Array.of_list)
           | [| x; y |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf
                        "drop: expected array and function, got %s, %s"
                        (type_of_value x) (type_of_value y),
                      { line = 0; column = 0 } ))
           | _ ->
               raise (Runtime_error ("Unreachable", { line = 0; column = 0 }))
       ));

  set_value env "split"
    (VFunction
       ( "split",
         2,
         fun _ args ->
           match args with
           | [| VStr s; VStr ss |] ->
               VArray
                 (Str.split (Str.regexp ss) s
                 |> List.map (fun a -> VStr a)
                 |> Array.of_list)
           | [| a; VStr _ |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf
                        "split: expected a string to split on, got %s"
                        (type_of_value a),
                      { line = 0; column = 0 } ))
           | [| VStr _; a |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf
                        "split: expected a string to split by, got %s"
                        (type_of_value a),
                      { line = 0; column = 0 } ))
           | [| a; b |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf
                        "split: expected a string to split, and string to \
                         split on, got %s, %s"
                        (type_of_value a) (type_of_value b),
                      { line = 0; column = 0 } ))
           | _ ->
               raise (Runtime_error ("Unreachable", { line = 0; column = 0 }))
       ));

  set_value env "parseNum"
    (VFunction
       ( "parseNum",
         1,
         fun _ args ->
           match args with
           | [| VStr s |] -> VNum (float_of_string s)
           | [| a |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf "parseNum: expected str, got %s"
                        (type_of_value a),
                      { line = 0; column = 0 } ))
           | _ ->
               raise (Runtime_error ("Unreachable", { line = 0; column = 0 }))
       ));

  set_value env "lines"
    (VFunction
       ( "lines",
         1,
         fun _ args ->
           match args with
           | [| VStr s |] -> (
               try
                 VArray
                   (In_channel.with_open_text s In_channel.input_lines
                   |> List.map (fun a -> VStr a)
                   |> Array.of_list)
               with Sys_error exn ->
                 raise
                   (Runtime_error
                      ( Printf.sprintf "lines: unable to open file: %s" exn,
                        { line = 0; column = 0 } )))
           | [| a |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf "lines: expected str, got %s"
                        (type_of_value a),
                      { line = 0; column = 0 } ))
           | _ ->
               raise (Runtime_error ("Unreachable", { line = 0; column = 0 }))
       ));

  set_value env "inc"
    (VFunction
       ( "inc",
         1,
         fun _ args ->
           match args with
           | [| VNum n |] -> VNum (n +. 1.)
           | [| a |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf "inc: expected num, got %s"
                        (type_of_value a),
                      { line = 0; column = 0 } ))
           | _ ->
               raise (Runtime_error ("Unreachable", { line = 0; column = 0 }))
       ));

  set_value env "dec"
    (VFunction
       ( "dec",
         1,
         fun _ args ->
           match args with
           | [| VNum n |] -> VNum (n -. 1.)
           | [| a |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf "dec: expected num, got %s"
                        (type_of_value a),
                      { line = 0; column = 0 } ))
           | _ ->
               raise (Runtime_error ("Unreachable", { line = 0; column = 0 }))
       ));

  set_value env "abs"
    (VFunction
       ( "abs",
         1,
         fun _ args ->
           match args with
           | [| VNum n |] -> VNum (n |> int_of_float |> abs |> float_of_int)
           | [| a |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf "abs: expected num, got %s"
                        (type_of_value a),
                      { line = 0; column = 0 } ))
           | _ ->
               raise (Runtime_error ("Unreachable", { line = 0; column = 0 }))
       ));
  { env }

(* Convert token to string for debugging *)
let string_of_position pos = Printf.sprintf "%d:%d" pos.line pos.column

(* Tokenizer state *)
type tokenizer = {
  input : string;
  mutable pos : int;
  mutable line : int;
  mutable column : int;
}

(* Create a new tokenizer *)
let create input = { input; pos = 0; line = 1; column = 1 }

(* Get current character *)
let current_char t =
  if t.pos >= String.length t.input then None
  else Some (String.get t.input t.pos)

let peek_char t =
  if t.pos + 1 >= String.length t.input then None
  else Some (String.get t.input (t.pos + 1))

(* Get current position *)
let current_position t = { line = t.line; column = t.column }

(* Advance position *)
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

(* Read a number *)
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

(* Read an identifier or keyword *)
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

(* Get next token *)
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
            | s -> Ident s)
        | '+' ->
            advance t;
            Plus
        | '-' ->
            advance t;
            Minus
        | '*' ->
            advance t;
            Star
        | '/' ->
            advance t;
            Slash
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

(* Tokenize entire input into a list *)
let tokenize input =
  let t = create input in
  let rec collect_tokens acc =
    match next_token t with
    | { kind = EOF; _ } as token -> List.rev (token :: acc)
    | token -> collect_tokens (token :: acc)
  in
  Array.of_list (collect_tokens [])

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
  | Minus ->
      let _ = consume p in
      let right = parse_term p in
      let node = EBinOp (Sub, left, right, token.pos) in
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
  | Slash ->
      let _ = consume p in
      let right = parse_orelse p in
      let node = EBinOp (Div, left, right, token.pos) in
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

(* Parse tokens into AST *)
let parse_repl tokens input =
  let p = create_parser tokens input in
  let ast = parse_stmt p in
  let final_token = peek p in
  if final_token.kind <> EOF then
    raise
      (Parse_error
         ( error_with_context p.input final_token.pos "Expected end of input",
           final_token.pos ));
  ast

let parse tokens input =
  let p = create_parser tokens input in
  let rec next_stmt acc =
    let ast = parse_stmt p in
    let final_token = peek p in
    if final_token.kind == EOF then List.rev (ast :: acc)
    else next_stmt (ast :: acc)
  in
  next_stmt []

let string_of_stmt = function
  | SDef (t, e) ->
      Printf.sprintf "def %s = %s"
        (match t.kind with Ident s -> s | _ -> "unknown")
        (string_of_expr e)
  | SExpr e -> string_of_expr e

let rec get_value e var pos =
  match Hashtbl.find_opt e.values var with
  | Some v -> v
  | None -> (
      match e.enclosing with
      | Some otherEnv -> get_value otherEnv var pos
      | None -> raise (Runtime_error ("Undefined variable", pos)))

let rec eval i (parent : stmt option) = function
  | ENum (n, _) -> VNum n
  | EBoolean (b, _) -> VBool b
  | EString (s, _) -> VStr s
  | ENil _ -> VNil
  | EArray (elems, _) ->
      let values =
        elems |> List.map (fun e -> eval i parent e) |> Array.of_list
      in
      VArray values
  | ERange (start_expr, end_expr, inclusive, pos) ->
      let start_val = eval i parent start_expr in
      let end_val = eval i parent end_expr in
      let start_num = to_num start_val pos in
      let end_num = to_num end_val pos in

      (* Check if numbers are integers *)
      if start_num <> Float.floor start_num || end_num <> Float.floor end_num
      then raise (Runtime_error ("Range bounds must be integers", pos));

      let start_int = int_of_float start_num in
      let end_int = int_of_float end_num in
      let end_adjusted = if inclusive then end_int else end_int - 1 in

      let rec make_range i acc =
        if i > end_adjusted then List.rev acc
        else make_range (i + 1) (VNum (float_of_int i) :: acc)
      in
      VArray (Array.of_list (make_range start_int []))
  | EUnaryMinus (e, pos) ->
      let v = eval i parent e in
      VNum (-.to_num v pos)
  | EUnaryNot (e, pos) ->
      let v = eval i parent e in
      v_not v pos
  | ECondition (condExpr, thenExpr, elseExpr, _) -> (
      let condResult = eval i parent condExpr in
      if is_truthy condResult then eval i parent thenExpr
      else match elseExpr with Some expr -> eval i parent expr | None -> VNil)
  | ESay (exprs, _) ->
      let stringified_exprs =
        List.map (fun e -> eval i parent e |> string_of_value) exprs
      in
      print_endline (String.concat "" stringified_exprs);
      VNil
  | ELetIn (bindings, letBody, _) ->
      (* Create a new closure, and execute *)
      let oldEnv = i.env in
      let newEnv = create_env (Some oldEnv) in
      i.env <- newEnv;
      bindings
      |> List.iter (fun (varName, varValue) ->
          match varName.kind with
          | Ident s -> set_value i.env s (eval i parent varValue)
          | _ ->
              raise
                (Runtime_error
                   ("Can only bind identifiers to values", varName.pos)));
      let rv = eval i parent letBody in
      i.env <- oldEnv;
      rv
  | EIdentifier (id, pos) -> get_value i.env id pos
  | EFn (params, body, _) ->
      (* Best-effort attempt to name the function in the environment properly.
       * For example, for the code:
       *
       *   def foo = \x => x + 1
       * or:
       *   defn foo(x) => x + 1
       *
       * We'll name it `foo', since that's what the user is binding it to. But, in this case:
       *
       *   (1..10).reduce(\acc, v => acc + v, 0)
       *
       * Since the lambda function isn't being binding to anything, we'll give it a generic name.
       *)
      let funcName =
        match parent with
        | Some (SDef ({ kind = Ident f; pos = _ }, _)) -> f
        | _ ->
            let next_fn = i.env.nFuncs in
            i.env.nFuncs <- next_fn + 1;
            Printf.sprintf "fn-%d" next_fn
      in

      (* Wrapper to execute the function. Creates a new environment for the closure. *)
      let funcBody i args =
        let oldEnv = i.env in
        let newEnv = create_env (Some i.env) in
        i.env <- newEnv;
        params
        |> List.iteri (fun i arg ->
            match arg.kind with
            | Ident s -> set_value newEnv s args.(i)
            | _ -> ());
        let rv = eval i parent body in
        i.env <- oldEnv;
        rv
      in

      let func = VFunction (funcName, List.length params, funcBody) in
      set_value i.env funcName func;
      func
  | ECall (funcNameExpr, paramsExprs, pos) -> (
      let callee = eval i parent funcNameExpr in

      (* Evaluate the parameters, for cases like: inc(1 + 1) *)
      let params =
        paramsExprs |> List.map (fun e -> eval i parent e) |> Array.of_list
      in

      match callee with
      | VFunction (funcName, arity, fn) ->
          (* Check the arity of the function *)
          if arity != Array.length params then
            raise
              (Runtime_error
                 ( Printf.sprintf
                     "Wrong number of args to %s, expected %d, got %d" funcName
                     arity (Array.length params),
                   pos ))
          else fn i params
      | _ -> VNil)
  | EBinOp (op, left, right, pos) -> (
      let left_val = eval i parent left in
      let right_val = eval i parent right in
      match op with
      | Add -> v_plus left_val right_val pos
      | Sub -> VNum (to_num left_val pos -. to_num right_val pos)
      | Mul -> VNum (to_num left_val pos *. to_num right_val pos)
      | Div ->
          let r = to_num right_val pos in
          if r = 0.0 then raise (Runtime_error ("Division by zero", pos))
          else VNum (to_num left_val pos /. r)
      | And -> VBool (to_bool left_val pos && to_bool right_val pos)
      | Or -> VBool (to_bool left_val pos || to_bool right_val pos)
      | Lt -> VBool (to_num left_val pos < to_num right_val pos)
      | Le -> VBool (to_num left_val pos <= to_num right_val pos)
      | Gt -> VBool (to_num left_val pos > to_num right_val pos)
      | Ge -> VBool (to_num left_val pos >= to_num right_val pos)
      | Eq -> v_eq left_val right_val pos
      | Ne -> v_ne left_val right_val pos
      | Modulo -> (
          match (left_val, right_val) with
          | VNum l, VNum r ->
              VNum (float_of_int (int_of_float l mod int_of_float r))
          | _ -> raise (Runtime_error ("Can only modulo numbers", pos)))
      | IntegerDiv -> (
          match (left_val, right_val) with
          | VNum l, VNum r ->
              VNum (float_of_int (int_of_float l / int_of_float r))
          | _ ->
              raise
                (Runtime_error ("Can only do integer division on numbers", pos))
          )
      | Spaceship -> (
          match (left_val, right_val) with
          | VNum x, VNum y ->
              VNum (float_of_int (if x < y then -1 else if x = y then 0 else 1))
          | _ ->
              raise
                (Runtime_error
                   ( Printf.sprintf "Unexpected `<=>` operands, got %s, %s"
                       (type_of_value left_val) (type_of_value right_val),
                     pos )))
      | ArrayConcat -> (
          match (left_val, right_val) with
          | VArray _, VArray _ ->
              raise
                (Runtime_error ("Array concatenation is with `+`, not `:`", pos))
          | VArray xs, y ->
              let new_xs = List.append (xs |> Array.to_list) [ y ] in
              VArray (Array.of_list new_xs)
          | x, VArray ys ->
              let new_ys = x :: (ys |> Array.to_list) in
              VArray (Array.of_list new_ys)
          | x, y ->
              raise
                (Runtime_error
                   ( Printf.sprintf "Unexpected `:` operands, got %s, %s"
                       (type_of_value x) (type_of_value y),
                     pos )))
      | OrElseNil -> (
          match (left_val, right_val) with VNil, a -> a | a, _ -> a))

let interpret i stmt =
  match stmt with
  | SDef (varName, varValue) ->
      (match varName.kind with
      | Ident s -> set_value i.env s (eval i (Some stmt) varValue)
      | _ ->
          raise
            (Runtime_error ("Can only bind identifiers to values", varName.pos)));
      None
  | SExpr e -> Some (eval i None e)

let repl () =
  let cliq_version = "0.1.0" in
  Printf.printf "Welcome to cliq v%s\nCtrl+C to exit\n" cliq_version;

  let env = create_env None in
  let interpreter = new_interpreter env in
  let rec loop () =
    Printf.printf "cliq> ";
    flush stdout;
    try
      let line = input_line stdin in
      let trimmed = String.trim line in

      if trimmed = "" then loop ()
      else
        try
          let tokens = tokenize trimmed in
          (* tokens |> Array.iter (fun t -> print_endline (string_of_token_kind t.kind)); *)
          let ast = parse_repl tokens trimmed in
          let result = interpret interpreter ast in
          (match result with
          | Some e ->
              Printf.printf ": %s = %s\n" (type_of_value e) (string_of_value e)
          | _ -> ());
          loop ()
        with
        | Tokenize_error (msg, pos) ->
            Printf.printf "Tokenize error at %s: %s\n\n"
              (string_of_position pos) msg;
            loop ()
        | Parse_error (msg, _) ->
            Printf.printf "Parse error:\n%s\n\n" msg;
            loop ()
        | Runtime_error (msg, pos) ->
            Printf.printf "Runtime error at %s: %s\n\n" (string_of_position pos)
              msg;
            loop ()
    with End_of_file -> Printf.printf "\nGoodbye!\n"
  in
  loop ()

let run_source_file input_file =
  let env = create_env None in
  let interpreter = new_interpreter env in
  try
    let code = In_channel.with_open_bin input_file In_channel.input_all in
    let tokens = tokenize code in
    (* tokens |> Array.iter (fun t -> print_endline (string_of_token_kind t.kind)); *)
    let stmts = parse tokens code in
    stmts
    |> List.iter (fun s ->
        let _ = interpret interpreter s in
        ())
  with
  | Tokenize_error (msg, pos) ->
      Printf.printf "Tokenize error at %s: %s\n\n" (string_of_position pos) msg
  | Parse_error (msg, _) -> Printf.printf "Parse error:\n%s\n\n" msg
  | Runtime_error (msg, pos) ->
      Printf.printf "Runtime error at %s: %s\n\n" (string_of_position pos) msg

(* Example usage *)
let () =
  (* Parse command line arguments *)
  let args = Array.to_list Sys.argv in
  match args with
  | _ :: "--test" :: _ ->
      let test_expressions =
        [
          "1 + 2";
          "3.14 * (5 - 2)";
          "10 / 2 + 3 * 4";
          "(1 + 2) * (3 + 4)";
          "1 + 2\n  * 3";
          "100 / (10 - 5 * 2)";
          "10 div 3 == 3";
          "10 mod 3 == 1";
          (* Logical expressions *)
          "true and false";
          "true or false";
          "not true";
          "not false and true";
          "(true or false) and not false";
          "true and true or false and false";
          (* Comparison expressions *)
          "5 > 3";
          "5 < 3";
          "5 >= 5";
          "5 <= 4";
          "5 == 5";
          "5 != 3";
          "true == true";
          "false != true";
          "(3 + 2) > 4";
          "10 / 2 <= 5";
          "1 <=> 10 == -1";
          "10 <=> 10 == 0";
          "20 <=> 10 == 1";
          (* Combined expressions *)
          "(5 > 3) and (2 < 4)";
          "not (10 == 5 * 2)";
          "(3 + 2) == 5 and 10 > 5";
          (* Strings *)
          "\"oh hay\"";
          "\"abc\" + \"def\" == \"abcdef\"";
          "\"abc\" + 1 == \"abc1\"";
          (* Let-in bindings *)
          "let x = 10 in x + 1";
          "let x = 10, y = 1 in x + y";
          (* Arrays *)
          "1 .. 5 == [1, 2, 3, 4, 5]";
          "1 ..< 5 == [1, 2, 3, 4]";
          "[] : 1 : 2 == [1, 2]";
          "1 : [2, 3] == [1, 2, 3]";
          "[1, 2] + [3, 4] == [1, 2, 3, 4]";
          (* Mixed - error cases *)
          "1 + true";
          "not 5";
          (* Other error cases *)
          "1 + ";
          "1 + + 2";
          "(1 + 2";
          "10 / 0";
          "let x = 10, in x + 1";
          "let x = 10 x + 1";
        ]
      in

      Printf.printf "Running tests...\n\n";
      List.iter
        (fun expr ->
          Printf.printf "Expression: %s\n" (String.escaped expr);
          try
            let tokens = tokenize expr in
            let ast = parse_repl tokens expr in
            Printf.printf "AST: %s\n" (string_of_stmt ast);
            let env = create_env None in
            let interpreter = new_interpreter env in
            let result = interpret interpreter ast in
            match result with
            | Some v -> Printf.printf "Result: %s\n\n" (string_of_value v)
            | _ -> ()
          with
          | Tokenize_error (msg, pos) ->
              Printf.printf "Tokenize error at %s: %s\n\n"
                (string_of_position pos) msg
          | Parse_error (msg, _) -> Printf.printf "Parse error:\n%s\n\n" msg
          | Runtime_error (msg, pos) ->
              Printf.printf "Runtime error at %s: %s\n\n"
                (string_of_position pos) msg)
        test_expressions;
      Printf.printf
        "\nTests complete. Run with --repl to start interactive mode.\n"
  | _ :: input_file :: _ -> run_source_file input_file
  | _ :: [] -> repl ()
  | _ -> ()
