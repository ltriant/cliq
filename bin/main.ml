open Intepreter
open Parser
open Types

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
          let tokens = Scanner.tokenize trimmed in
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
  try
    let code = In_channel.with_open_bin input_file In_channel.input_all in
    let env = create_env None in
    let interpreter = new_interpreter env in
    let tokens = Scanner.tokenize code in
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
            let tokens = Scanner.tokenize expr in
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
