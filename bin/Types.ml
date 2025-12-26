(* Position information for tokens *)
type position = { line : int; column : int }

(* Exception for tokenization errors *)
exception Tokenize_error of string * position

(* Exception for parsing errors *)
exception Parse_error of string * position

(* Exception for runtime errors *)
exception Runtime_error of string * position

(* Token type with position information *)
type token = { kind : token_kind; pos : position }

and token_kind =
  | Number of float
  | Bool of bool
  | Str of string
  | Ident of string
  | Plus
  | PlusPlus
  | Minus
  | MinusMinus
  | Star
  | StarStar
  | Slash
  | SlashSlash
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
  | LBrace
  | RBrace
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
  | Pow
  | LessEqualGreater
  | Colon
  | SlashQuestion
  | EqualSquiggle
  | EOF

let string_of_token_kind = function
  | Number n -> Printf.sprintf "Number(%g)" n
  | Bool b -> Printf.sprintf "Bool(%b)" b
  | Str s -> Printf.sprintf "String(%s)" s
  | Nil -> "ENil"
  | Ident i -> i
  | Plus -> "+"
  | PlusPlus -> "++"
  | Minus -> "-"
  | MinusMinus -> "--"
  | Star -> "*"
  | StarStar -> "**"
  | Slash -> "/"
  | SlashSlash -> "//"
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
  | LBrace -> "{"
  | RBrace -> "}"
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
  | Pow -> "pow"
  | LessEqualGreater -> "<=>"
  | Colon -> ":"
  | SlashQuestion -> "/?"
  | EqualSquiggle -> "=~"
  | EOF -> "EOF"

