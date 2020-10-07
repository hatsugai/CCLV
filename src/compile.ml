open Printf
open Error
open Utils
open S
open Ir

type cenv = { var_list : Id.t list; }

let cenv0 = { var_list = []; }

let h (expr : S.expr) : Ir.operand =
  match expr with
  | Int k -> Int k
  | Var x -> Var x
  | _ -> error_s "operand" (show_expr expr)

let rec compile cenv stmt cont =
  let is = compile1 cenv stmt cont in
  (if !Option.debug then
     begin
       printf "----------------------------------------\n";
       List.iteri
         (fun i inst -> printf "%d %s\n" i (Ir.show inst))
         is;
       printf "----------------------------------------\n";
     end);
  is

and compile1 cenv stmt cont =
  match stmt with
  | Skip -> cont
  | Assign (var, expr) ->
     compile_assign cenv var expr cont
  | Store (e1, e2) -> (Store (h e1, h e2))::cont
  | If (test, stmt1, stmt2) ->
     compile_if cenv test stmt1 stmt2 cont
  | While (test, stmt) ->
     error_s "found raw while" (S.show_statement stmt)
  | WhileS (pre_test_stmt, test, stmt) ->
     compile_while cenv pre_test_stmt test stmt cont
  | Compound stmt_list ->
     List.fold_right (compile cenv) stmt_list cont
  | Return expr ->
     (Return (h expr))::cont

and compile_assign cenv var expr cont =
  match expr with
  | Int k -> (Assign (var, h expr))::cont
  | Var x -> (Assign (var, h expr))::cont
  | Ref x -> (Load (var, h x))::cont
  | Call (f, args) -> (Call (var, f, List.map h args))::cont
  | Uniop (op, e) -> (Uniop (var, op, h e))::cont
  | Binop (op, e1, e2) ->
     (* ### And Or Eq Lt Le *)
     (Binop (var, op, h e1, h e2))::cont

and compile_if cenv test stmt1 stmt2 cont =
  let l1 = alloc_label () in
  let l2 = alloc_label () in
  let l3 = alloc_label () in
  let cont = (Label l2)::(compile cenv stmt2 ((Label l3)::cont)) in
  let cont = (Label l1)::(compile cenv stmt1 ((Bra l3)::cont)) in
  compile_cond cenv test l1 l2 cont

and compile_cond cenv test_expr label_true label_false cont =
  match test_expr with
  | Binop (op, e1, e2) ->
     (match op with
      | Eq ->
         if next_is_label cont label_true then
           (Bne (label_false, h e1, h e2))::cont
         else if next_is_label cont label_false then
           (Beq (label_true, h e1, h e2))::cont
         else
           (Beq (label_true, h e1, h e2))::(Bra label_false)::cont
      | Lt ->
         if next_is_label cont label_true then
           (Bge (label_false, h e1, h e2))::cont
         else if next_is_label cont label_false then
           (Blt (label_true, h e1, h e2))::cont
         else
           (Blt (label_true, h e1, h e2))::(Bra label_false)::cont
      | Le ->
         if next_is_label cont label_true then
           (Bgt (label_false, h e1, h e2))::cont
         else if next_is_label cont label_false then
           (Ble (label_true, h e1, h e2))::cont
         else
           (Ble (label_true, h e1, h e2))::(Bra label_false)::cont
      | And ->
         let l = alloc_label () in
         let cont = compile_cond cenv e2 label_true label_false cont in
         let cont = (Label l)::cont in
         compile_cond cenv e1 l label_false cont
      | Or ->
         let l = alloc_label () in
         let cont = compile_cond cenv e2 label_true label_false cont in
         let cont = (Label l)::cont in
         compile_cond cenv e1 label_true l cont
      | _ ->
         error_s "compile_cond" (show_expr test_expr))
  | Int k ->
     if k = 0 then
       if next_is_label cont label_false then
         cont
       else
         (Bra label_false)::cont
     else
       if next_is_label cont label_true then
         cont
       else
         (Bra label_true)::cont
  | Var _ ->
     if next_is_label cont label_true then
       (Bf (label_false, h test_expr))::cont
     else if next_is_label cont label_false then
       (Bt (label_true, h test_expr))::cont
     else
       (Bf (label_false, h test_expr))::(Bra label_true)::cont
  | _ ->
     error_s "compile_cond" (show_expr test_expr)

and next_is_label cont label =
  match cont with
  | (Label l)::_ -> l = label
  | _ -> false

and compile_while cenv pre_test_stmt test stmt cont =
  (*
    L1:
      pre_test_stmt_list
      if test then L2 else L3
    L2:
      stmt
      jmp L1
    L3:
   *)
  let l1 = alloc_label () in
  let l2 = alloc_label () in
  let l3 = alloc_label () in
  let cont = (Label l2)::(compile cenv stmt ((Bra l1)::(Label l3)::cont)) in
  let cont = compile_cond cenv test l2 l3 cont in
  (Label l1)::(compile cenv pre_test_stmt cont)
