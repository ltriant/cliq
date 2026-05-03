open Parser
open Types

type resolver = {
  interpreter : Interpreter.interpreter;
  mutable scopes : (string, bool) Hashtbl.t Dynarray.t;
  errors : (string * position) Dynarray.t;
}

let new_resolver i =
  { interpreter = i; scopes = Dynarray.create (); errors = Dynarray.create () }

let begin_scope r =
  let new_scope = Hashtbl.create 10 in
  Dynarray.add_last r.scopes new_scope

let end_scope r =
  let _ = Dynarray.pop_last r.scopes in
  ()

let define_val r v =
  match v.kind with
  | Ident s ->
      let scope = Dynarray.get_last r.scopes in
      Hashtbl.add scope s true
  | _ -> ()

let register_builtin_functions r =
  let scope = Dynarray.get_last r.scopes in
  List.iter
    (fun s -> Hashtbl.add scope s true)
    [
      "length";
      "reverse";
      "map";
      "filter";
      "reduce";
      "zip";
      "sum";
      "product";
      "first";
      "second";
      "last";
      "rest";
      "drop";
      "dropLast";
      "sortBy";
      "sort";
      "keys";
      "values";
      "split";
      "words";
      "parseNum";
      "lines";
      "slurp";
      "inc";
      "dec";
      "abs";
      "assoc";
      "dissoc";
      "update";
    ]

let rec resolve_expr r e =
  match e with
  | ENum _ -> ()
  | EBoolean _ -> ()
  | EString _ -> ()
  | ENil _ -> ()
  | EIdentifier (id, pos) -> (
      match Dynarray.exists (fun scope -> Hashtbl.mem scope id) r.scopes with
      | false -> Dynarray.add_last r.errors ("Undefined variable", pos)
      | _ -> ())
  | EArray (xs, _) -> List.iter (resolve_expr r) xs
  | EMap (kvs, _) ->
      List.iter
        (fun (k, v) ->
          resolve_expr r k;
          resolve_expr r v)
        kvs
  | EIndex (e1, e2, _) ->
      resolve_expr r e1;
      resolve_expr r e2
  | ERange (e1, e2, _, _) ->
      resolve_expr r e1;
      resolve_expr r e2
  | EBinOp (_, e1, e2, _) ->
      resolve_expr r e1;
      resolve_expr r e2
  | EUnaryMinus (e, _) -> resolve_expr r e
  | EUnaryNot (e, _) -> resolve_expr r e
  | ECondition (e1, e2, e3_opt, _) -> (
      resolve_expr r e1;
      resolve_expr r e2;
      match e3_opt with Some e -> resolve_expr r e | _ -> ())
  | ESay (xs, _) -> List.iter (resolve_expr r) xs
  | ELetIn (vs, e, _) ->
      begin_scope r;
      List.iter
        (fun (t, e) ->
          define_val r t;
          resolve_expr r e)
        vs;
      resolve_expr r e;
      end_scope r
  | EFn (xs, e, _) ->
      begin_scope r;
      List.iter (define_val r) xs;
      resolve_expr r e;
      end_scope r
  | ECall (e, es, _) ->
      resolve_expr r e;
      List.iter (resolve_expr r) es

let resolve_stmt r s =
  match s with
  | SDef (v, e) ->
      define_val r v;
      resolve_expr r e
  | SExpr e -> resolve_expr r e

let resolve r stmts =
  begin_scope r;
  register_builtin_functions r;
  List.iter (resolve_stmt r) stmts;
  end_scope r;

  if Dynarray.length r.errors > 0 then raise (Resolve_error r.errors)
