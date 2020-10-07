open Printf
open Utils
open S

let alloc_var () =
  Id.get_temp ()

let rec is_simple expr =
  match expr with
  | Int _ | Var _ -> true
  | _ -> false

let assign stmt_list expr =
  if is_simple expr then
    (stmt_list, expr)
  else
    let x = alloc_var () in
    (Assign (x, expr)::stmt_list, Var x)

let rec norm_expr0 stmt_list expr =
  match expr with
  | Int k ->
     (stmt_list, expr)
  | Var x ->
     (stmt_list, expr)
  | Ref e ->
     let (stmt_list, x) = norm_expr stmt_list e in
     (stmt_list, Ref x)
  | Call (f, args) ->
     let (stmt_list, xs) = fold_map norm_expr stmt_list args in
     (stmt_list, Call (f, xs))
  | Uniop (op, e) ->
     let (stmt_list, x) = norm_expr stmt_list e in
     (stmt_list, Uniop (op, x))
  | Binop (op, e1, e2) ->
     (match op with
      | And | Or ->
         let (stmt_list, x) = norm_expr_comp stmt_list e1 in
         let (stmt_list, y) = norm_expr_comp stmt_list e2 in
         (stmt_list, Binop (op, x, y))
      | _ ->
         let (stmt_list, x) = norm_expr stmt_list e1 in
         let (stmt_list, y) = norm_expr stmt_list e2 in
         (stmt_list, Binop (op, x, y)))

and norm_expr stmt_list expr =
  let (stmt_list, x) = norm_expr0 stmt_list expr in
  assign stmt_list x

and norm_expr_comp stmt_list expr =
  match expr with
  | Binop (op, e1, e2) ->
     (match op with
      | And | Or ->
         let (stmt_list, x) = norm_expr0 stmt_list e1 in
         let (stmt_list, y) = norm_expr0 stmt_list e2 in
         (stmt_list, Binop (op, x, y))
      | Lt | Le | Eq ->
         let (stmt_list, x) = norm_expr stmt_list e1 in
         let (stmt_list, y) = norm_expr stmt_list e2 in
         (stmt_list, Binop (op, x, y))
      | _ -> norm_expr stmt_list expr)
  | _ -> norm_expr stmt_list expr

let rec norm stmt_list stmt =
  match stmt with
  | Skip -> stmt_list
  | Assign (var, expr) ->
     let (stmt_list, x) = norm_expr0 stmt_list expr in
     Assign (var, x)::stmt_list
  | Store (loc, expr) ->
     let (stmt_list, x) = norm_expr stmt_list loc in
     let (stmt_list, y) = norm_expr stmt_list expr in
     (Store (x, y))::stmt_list
  | If (test, stmt_true, stmt_false) ->
     let (stmt_list, x) = norm_expr0 stmt_list test in
     let stmt_list_true = List.rev (norm [] stmt_true) in
     let stmt_list_false = List.rev (norm [] stmt_false) in
     (If (x, Compound stmt_list_true, Compound stmt_list_false))::stmt_list
  | While (test, body) ->
     let (pre_test_stmt_list, x) = norm_expr0 [] test in
     let ss_body = List.rev (norm [] body) in
     (WhileS (Compound pre_test_stmt_list, x, Compound ss_body))::stmt_list
  | WhileS (pre_test_stmt, test, body) ->
     stmt::stmt_list
  | Compound ss ->
     List.fold_left norm stmt_list ss
  | Return expr ->
     let (stmt_list, x) = norm_expr stmt_list expr in
     (Return x)::stmt_list

let normalize fun_def =
  let stmt_list = norm [] fun_def.body in
  let stmt = Compound (List.rev stmt_list) in
  (if !Option.debug then
     begin
       printf "%s: %s\n" (Id.show fun_def.name) (show_statement stmt)
     end);
  stmt
