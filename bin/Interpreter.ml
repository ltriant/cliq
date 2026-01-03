open Parser
open Types

let words_pattern = Re.Perl.re " +" |> Re.compile

type value =
  | VNum of float
  | VBool of bool
  | VStr of string
  | VArray of value array
  | VMap of (value, value) Hashtbl.t
  | VFunction of
      string * int * (interpreter -> position -> value array -> value)
  | VNil

(* Interpreter environment *)
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
  | VMap _ -> "map"
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

let v_array_plus a b pos =
  match (a, b) with
  | VArray xs, _ -> VArray (xs |> Array.map (fun x -> v_plus x b pos))
  | a, _ ->
      raise
        (Runtime_error
           ( Printf.sprintf "Expected array as left val of `++`, got %s"
               (type_of_value a),
             pos ))

let v_sub a b pos = VNum (to_num a pos -. to_num b pos)

let v_array_sub a b pos =
  match (a, b) with
  | VArray xs, _ -> VArray (xs |> Array.map (fun x -> v_sub x b pos))
  | a, _ ->
      raise
        (Runtime_error
           ( Printf.sprintf "Expected array as left val of `--`, got %s"
               (type_of_value a),
             pos ))

let v_mul a b pos = VNum (to_num a pos *. to_num b pos)

let v_array_mul a b pos =
  match (a, b) with
  | VArray xs, _ -> VArray (xs |> Array.map (fun x -> v_mul x b pos))
  | a, _ ->
      raise
        (Runtime_error
           ( Printf.sprintf "Expected array as left val of `**`, got %s"
               (type_of_value a),
             pos ))

let v_div a b pos =
  let r = to_num b pos in
  if r = 0.0 then raise (Runtime_error ("Division by zero", pos))
  else VNum (to_num a pos /. r)

let v_array_div a b pos =
  match (a, b) with
  | VArray xs, _ -> VArray (xs |> Array.map (fun x -> v_div x b pos))
  | a, _ ->
      raise
        (Runtime_error
           ( Printf.sprintf "Expected array as left val of `//`, got %s"
               (type_of_value a),
             pos ))

let rec v_int_div a b pos =
  match (a, b) with
  | VNum l, VNum r -> VNum (float_of_int (int_of_float l / int_of_float r))
  | VArray xs, _ -> VArray (xs |> Array.map (fun x -> v_int_div x b pos))
  | _ -> raise (Runtime_error ("Can only do integer division on numbers", pos))

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
  | VMap _, VMap _ ->
      (* TODO calc this manually *)
      VBool false
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

let v_idx v i _ =
  match (v, i) with
  | VArray xs, VNum i ->
      let n_items = Array.length xs in
      let n = int_of_float i in
      if n < 0 then xs.(n_items - abs n) else xs.(n)
  | VMap m, s -> ( match Hashtbl.find_opt m s with Some e -> e | None -> VNil)
  | _, _ -> VNil

let is_truthy v =
  match v with
  | VNum n -> n > 0.0
  | VBool b -> b
  | VStr s -> String.length s > 0
  | VArray a -> Array.length a > 0
  | VNil -> false
  | VFunction _ -> false
  | VMap m -> Hashtbl.length m > 0

(* Convert value to string for printing *)
let rec string_of_value = function
  | VNum n ->
      let rv = Printf.sprintf "%f" n in
      if String.ends_with ~suffix:".000000" rv then Printf.sprintf "%.0f" n
      else rv
  | VBool b -> string_of_bool b
  | VStr s -> s
  | VArray elems ->
      let elem_strs = elems |> Array.map string_of_value |> Array.to_list in
      "[" ^ String.concat ", " elem_strs ^ "]"
  | VMap elems ->
      let elem_strs =
        elems |> Hashtbl.to_seq
        |> Seq.map (fun (k, v) ->
            Printf.sprintf "%s => %s" (string_of_value k) (string_of_value v))
        |> List.of_seq
      in
      "{ " ^ String.concat ", " elem_strs ^ " }"
  | VNil -> "nil"
  | VFunction (n, _, _) -> Printf.sprintf "<fn %s>" n

let create_env enclosing = { values = Hashtbl.create 10; nFuncs = 0; enclosing }
let set_value e var value = Hashtbl.replace e.values var value

let new_interpreter env =
  set_value env "length"
    (VFunction
       ( "length",
         1,
         fun _ pos args ->
           match args with
           | [| VNil |] -> VNil
           | [| VArray xs |] -> VNum (xs |> Array.length |> float_of_int)
           | [| VMap kvs |] -> VNum (Hashtbl.length kvs |> float_of_int)
           | [| VStr s |] -> VNum (s |> String.length |> float_of_int)
           | _ -> raise (Runtime_error ("length: expected array", pos)) ));

  set_value env "reverse"
    (VFunction
       ( "reverse",
         1,
         fun _ pos args ->
           match args with
           | [| VNil |] -> VNil
           | [| VArray xs |] ->
               VArray (xs |> Array.to_list |> List.rev |> Array.of_list)
           | _ -> raise (Runtime_error ("reverse: expected array", pos)) ));

  set_value env "map"
    (VFunction
       ( "map",
         2,
         fun i pos args ->
           match args with
           | [| VNil |] -> VNil
           | [| VArray xs; VFunction (_, arity, fn) |] ->
               if arity <> 1 then
                 raise
                   (Runtime_error
                      ( "map: function should have arity of 1 when applied to \
                         an array",
                        pos ))
               else VArray (xs |> Array.map (fun x -> fn i pos [| x |]))
           | [| VMap xs; VFunction (_, arity, fn) |] ->
               if arity <> 2 then
                 raise
                   (Runtime_error
                      ( "map: function should have arity of 2 when applied to \
                         a map",
                        pos ))
               else
                 VMap
                   (xs |> Hashtbl.to_seq
                   |> Seq.map (fun (k, v) -> (k, fn i pos [| k; v |]))
                   |> Hashtbl.of_seq)
           | [| VMap _ | VArray _; a |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf "map: expected a function to apply, got %s"
                        (type_of_value a),
                      pos ))
           | [| a; VFunction _ |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf
                        "map: expected a collection to map over, got %s"
                        (type_of_value a),
                      pos ))
           | _ ->
               raise (Runtime_error ("map: expected array, and function", pos))
       ));

  set_value env "filter"
    (VFunction
       ( "filter",
         2,
         fun i pos args ->
           match args with
           | [| VNil |] -> VNil
           | [| VArray xs; VFunction (_, arity, fn) |] ->
               if arity != 1 then
                 raise
                   (Runtime_error
                      ( "filter: function should have arity of 1 when applied \
                         to an array",
                        pos ))
               else
                 VArray
                   (xs |> Array.to_list
                   |> List.filter (fun x -> is_truthy (fn i pos [| x |]))
                   |> Array.of_list)
           | [| VMap xs; VFunction (_, arity, fn) |] ->
               if arity <> 2 then
                 raise
                   (Runtime_error
                      ( "filter: function should have arity of 2 when applied \
                         to a map",
                        pos ))
               else
                 VMap
                   (xs |> Hashtbl.to_seq
                   |> Seq.filter (fun (k, v) -> is_truthy (fn i pos [| k; v |]))
                   |> Hashtbl.of_seq)
           | [| VArray _ | VMap _; a |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf
                        "filter: expected a function to apply, got %s"
                        (type_of_value a),
                      pos ))
           | [| a; VFunction _ |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf "filter: expected array to filter, got %s"
                        (type_of_value a),
                      pos ))
           | _ ->
               raise
                 (Runtime_error ("filter: expected array, and function", pos))
       ));

  set_value env "reduce"
    (VFunction
       ( "reduce",
         3,
         fun i pos args ->
           match args with
           | [| VNil |] -> VNil
           | [| VArray xs; VFunction (_, arity, fn); init |] ->
               if arity != 2 then
                 raise
                   (Runtime_error
                      ( "reduce: function should have arity of 2 when applied \
                         to an array",
                        pos ))
               else
                 xs |> Array.to_list
                 |> List.fold_left (fun acc v -> fn i pos [| acc; v |]) init
           | [| VMap xs; VFunction (_, arity, fn); init |] ->
               if arity != 3 then
                 raise
                   (Runtime_error
                      ( "reduce: function should have arity of 3 when applied \
                         to a map",
                        pos ))
               else
                 xs |> Hashtbl.to_seq
                 |> Seq.fold_left
                      (fun acc (k, v) -> fn i pos [| acc; k; v |])
                      init
           | [| VArray _ | VMap _; a; _ |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf
                        "reduce: expected a function for reduce, got %s"
                        (type_of_value a),
                      pos ))
           | [| a; VFunction _; _ |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf
                        "reduce: expected an array to reduce, got %s"
                        (type_of_value a),
                      pos ))
           | _ ->
               raise
                 (Runtime_error
                    ("reduce: expected array, function, and initial value", pos))
       ));

  set_value env "zip"
    (VFunction
       ( "zip",
         2,
         fun _ pos args ->
           match args with
           | [| VNil; _ |] -> VNil
           | [| VArray xs; VArray ys |] ->
               if Array.length xs != Array.length ys then
                 raise
                   (Runtime_error
                      ( Printf.sprintf
                          "zip: expected arrays of same length, got %d, %d"
                          (Array.length xs) (Array.length ys),
                        pos ))
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
                      pos ))
           | _ -> raise (Runtime_error ("Unreachable", pos)) ));

  set_value env "sum"
    (VFunction
       ( "sum",
         1,
         fun _ pos args ->
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
                                   pos )))
                      0.)
           | [| a |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf "sum: expected array, got %s"
                        (type_of_value a),
                      pos ))
           | _ -> raise (Runtime_error ("Unreachable", pos)) ));

  set_value env "first"
    (VFunction
       ( "first",
         1,
         fun _ pos args ->
           match args with
           | [| VNil |] -> VNil
           | [| VArray xs |] -> if Array.length xs > 0 then xs.(0) else VNil
           | [| x |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf "first: expected array, got %s"
                        (type_of_value x),
                      pos ))
           | _ -> raise (Runtime_error ("Unreachable", pos)) ));

  set_value env "second"
    (VFunction
       ( "second",
         1,
         fun _ pos args ->
           match args with
           | [| VNil |] -> VNil
           | [| VArray xs |] -> if Array.length xs > 1 then xs.(1) else VNil
           | [| x |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf "second: expected array, got %s"
                        (type_of_value x),
                      pos ))
           | _ -> raise (Runtime_error ("Unreachable", pos)) ));

  set_value env "last"
    (VFunction
       ( "last",
         1,
         fun _ pos args ->
           match args with
           | [| VNil |] -> VNil
           | [| VArray xs |] ->
               if Array.length xs > 0 then xs.(Array.length xs - 1) else VNil
           | [| x |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf "last: expected array, got %s"
                        (type_of_value x),
                      pos ))
           | _ -> raise (Runtime_error ("Unreachable", pos)) ));

  set_value env "rest"
    (VFunction
       ( "rest",
         1,
         fun _ pos args ->
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
                      pos ))
           | _ -> raise (Runtime_error ("Unreachable", pos)) ));

  set_value env "drop"
    (VFunction
       ( "drop",
         2,
         fun _ pos args ->
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
                      pos ))
           | _ -> raise (Runtime_error ("Unreachable", pos)) ));

  set_value env "dropLast"
    (VFunction
       ( "dropLast",
         2,
         fun _ pos args ->
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
                      pos ))
           | _ -> raise (Runtime_error ("Unreachable", pos)) ));

  set_value env "sort"
    (VFunction
       ( "sort",
         2,
         fun i pos args ->
           match args with
           | [| VNil; _ |] -> VNil
           | [| VArray xs; VFunction (_, arity, fn) |] ->
               if arity != 2 then
                 raise
                   (Runtime_error
                      ("reduce: function should have arity of 2", pos))
               else
                 VArray
                   (xs |> Array.to_list
                   |> List.sort (fun a b ->
                       match fn i pos [| a; b |] with
                       | VNum n -> int_of_float n
                       | a ->
                           raise
                             (Runtime_error
                                ( Printf.sprintf
                                    "Expected sort function to return num, got \
                                     %s"
                                    (type_of_value a),
                                  pos )))
                   |> Array.of_list)
           | [| x; y |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf
                        "drop: expected array and function, got %s, %s"
                        (type_of_value x) (type_of_value y),
                      pos ))
           | _ -> raise (Runtime_error ("Unreachable", pos)) ));

  set_value env "keys"
    (VFunction
       ( "keys",
         1,
         fun _ pos args ->
           match args with
           | [| VMap xs |] -> VArray (xs |> Hashtbl.to_seq_keys |> Array.of_seq)
           | [| x |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf "keys: expected map, got %s"
                        (type_of_value x),
                      pos ))
           | _ -> raise (Runtime_error ("Unreachable", pos)) ));

  set_value env "values"
    (VFunction
       ( "values",
         1,
         fun _ pos args ->
           match args with
           | [| VMap xs |] ->
               VArray (xs |> Hashtbl.to_seq_values |> Array.of_seq)
           | [| x |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf "values: expected map, got %s"
                        (type_of_value x),
                      pos ))
           | _ -> raise (Runtime_error ("Unreachable", pos)) ));

  set_value env "split"
    (VFunction
       ( "split",
         2,
         fun _ pos args ->
           match args with
           | [| VStr s; VStr ss |] ->
               let patt = Re.Perl.re ss |> Re.compile in
               VArray
                 (Re.split patt s |> List.map (fun a -> VStr a) |> Array.of_list)
           | [| a; VStr _ |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf
                        "split: expected a string to split on, got %s"
                        (type_of_value a),
                      pos ))
           | [| VStr _; a |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf
                        "split: expected a string to split by, got %s"
                        (type_of_value a),
                      pos ))
           | [| a; b |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf
                        "split: expected a string to split, and string to \
                         split on, got %s, %s"
                        (type_of_value a) (type_of_value b),
                      pos ))
           | _ -> raise (Runtime_error ("Unreachable", pos)) ));

  set_value env "words"
    (VFunction
       ( "words",
         1,
         fun _ pos args ->
           match args with
           | [| VStr s |] ->
               VArray
                 (Re.split words_pattern s
                 |> List.map (fun a -> VStr a)
                 |> Array.of_list)
           | [| a |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf "words: expected a string to split, got %s"
                        (type_of_value a),
                      pos ))
           | _ -> raise (Runtime_error ("Unreachable", pos)) ));

  set_value env "parseNum"
    (VFunction
       ( "parseNum",
         1,
         fun _ pos args ->
           match args with
           | [| VStr s |] -> VNum (float_of_string s)
           | [| a |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf "parseNum: expected str, got %s"
                        (type_of_value a),
                      pos ))
           | _ -> raise (Runtime_error ("Unreachable", pos)) ));

  set_value env "lines"
    (VFunction
       ( "lines",
         1,
         fun _ pos args ->
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
                      (Printf.sprintf "lines: unable to open file: %s" exn, pos))
               )
           | [| a |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf "lines: expected str, got %s"
                        (type_of_value a),
                      pos ))
           | _ -> raise (Runtime_error ("Unreachable", pos)) ));

  set_value env "slurp"
    (VFunction
       ( "slurp",
         1,
         fun _ pos args ->
           match args with
           | [| VStr s |] -> (
               try VStr (In_channel.with_open_text s In_channel.input_all)
               with Sys_error exn ->
                 raise
                   (Runtime_error
                      (Printf.sprintf "slurp: unable to open file: %s" exn, pos))
               )
           | [| a |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf "slurp: expected str, got %s"
                        (type_of_value a),
                      pos ))
           | _ -> raise (Runtime_error ("Unreachable", pos)) ));

  set_value env "inc"
    (VFunction
       ( "inc",
         1,
         fun _ pos args ->
           match args with
           | [| VNum n |] -> VNum (n +. 1.)
           | [| a |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf "inc: expected num, got %s"
                        (type_of_value a),
                      pos ))
           | _ -> raise (Runtime_error ("Unreachable", pos)) ));

  set_value env "dec"
    (VFunction
       ( "dec",
         1,
         fun _ pos args ->
           match args with
           | [| VNum n |] -> VNum (n -. 1.)
           | [| a |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf "dec: expected num, got %s"
                        (type_of_value a),
                      pos ))
           | _ -> raise (Runtime_error ("Unreachable", pos)) ));

  set_value env "abs"
    (VFunction
       ( "abs",
         1,
         fun _ pos args ->
           match args with
           | [| VNum n |] -> VNum (n |> int_of_float |> abs |> float_of_int)
           | [| a |] ->
               raise
                 (Runtime_error
                    ( Printf.sprintf "abs: expected num, got %s"
                        (type_of_value a),
                      pos ))
           | _ -> raise (Runtime_error ("Unreachable", pos)) ));

  { env }

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
  | EMap (elems, _) ->
      let kvs =
        elems |> List.to_seq
        |> Seq.map (fun (k, v) ->
            let k_val = eval i parent k in
            let v_val = eval i parent v in
            (k_val, v_val))
        |> Hashtbl.of_seq
      in
      VMap kvs
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
  | EIndex (collectionExpr, idxExpr, pos) ->
      let coll = eval i parent collectionExpr in
      let idx = eval i parent idxExpr in
      v_idx coll idx pos
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
      let funcBody i _ args =
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
          else fn i pos params
      | _ -> VNil)
  | EBinOp (op, left, right, pos) -> (
      let left_val = eval i parent left in
      let right_val = eval i parent right in
      match op with
      | Add -> v_plus left_val right_val pos
      | ArrayAdd -> v_array_plus left_val right_val pos
      | Sub -> v_sub left_val right_val pos
      | ArraySub -> v_array_sub left_val right_val pos
      | Mul -> v_mul left_val right_val pos
      | ArrayMul -> v_array_mul left_val right_val pos
      | Div -> v_div left_val right_val pos
      | ArrayDiv -> v_array_div left_val right_val pos
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
      | IntegerDiv -> v_int_div left_val right_val pos
      | Power -> (
          match (left_val, right_val) with
          | VNum l, VNum r -> VNum (l ** r)
          | _ -> raise (Runtime_error ("Can only do power on numbers", pos)))
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
          match (left_val, right_val) with VNil, a -> a | a, _ -> a)
      | IsDivisible -> (
          match (left_val, right_val) with
          | VNum a, VNum b -> VBool (int_of_float a mod int_of_float b == 0)
          | a, b ->
              raise
                (Runtime_error
                   ( Printf.sprintf "Unexpected `/?` operands, got %s, %s"
                       (type_of_value a) (type_of_value b),
                     pos )))
      | RegexMatch -> VNil)

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
