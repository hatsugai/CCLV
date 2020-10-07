open Printf
open Utils
open Error
open Col
open IdCol
open Ir

let num_registers = 64

let operand_var acc operand =
  match operand with
  | Var x -> IdSet.add x acc
  | Int _ -> acc

let collect_vars_inst acc inst =
  match inst with
  | Label k -> acc
  | Assign (x, y) -> IdSet.add x (operand_var acc y)
  | Load (x, y) -> IdSet.add x (operand_var acc y)
  | Store (x, y) -> operand_var (operand_var acc x) y
  | Call (x, f, args) -> List.fold_left operand_var (IdSet.add x acc) args
  | Uniop (x, op, y) -> IdSet.add x (operand_var acc y)
  | Binop (x, op, y, z) -> IdSet.add x (operand_var (operand_var acc y) z)
  | Bf (label, x) -> operand_var acc x
  | Bt (label, x) -> operand_var acc x
  | Bra label -> acc
  | Beq (label, x, y) -> operand_var (operand_var acc x) y
  | Bne (label, x, y) -> operand_var (operand_var acc x) y
  | Blt (label, x, y) -> operand_var (operand_var acc x) y
  | Ble (label, x, y) -> operand_var (operand_var acc x) y
  | Bgt (label, x, y) -> operand_var (operand_var acc x) y
  | Bge (label, x, y) -> operand_var (operand_var acc x) y
  | Return x -> operand_var acc x

let collect_vars acc is =
  List.fold_left collect_vars_inst acc is

(*
  liferange: Id.t -> int * int
 *)
let calc_life_range1 params is =
  let liferange =
    List.fold_left
      (fun liferange param -> IdMap.add param (0, 0) liferange)
      IdMap.empty params
  in
  List.fold_lefti
    (fun liferange i inst ->
      let vars = collect_vars_inst IdSet.empty inst in
      IdSet.fold
        (fun var liferange ->
          match IdMap.find_opt var liferange with
          | None -> IdMap.add var (i, i) liferange
          | Some (a, b) -> IdMap.add var (a, i) liferange)
        vars liferange)
    liferange is

let calc_life_range params is =
  let liferange = calc_life_range1 params is in
  (if !Option.debug then
     IdMap.iter
       (fun var (a, b) -> printf "%s (%d, %d)\n" (Id.show var) a b)
       liferange);
  liferange

(*
  intervals: (var, (a, b)) list, sorted by a
  active: (reg_id, (a, b)) list
  free: reg_id list
*)

let print_regmap regmap =
  IdMap.iter
    (fun var reg_id -> printf "(%s, %d) " (Id.show var) reg_id)
    regmap;
  printf "\n"

let print_active active =
  List.iter
    (fun (regid, (a, b)) ->
      printf "(%d, (%d, %d)) " regid a b)
    active;
  printf "\n"

let print_free free =
  List.iter
    (fun regid ->
      printf "%d " regid)
    free;
  printf "\n"

let reg_alloc1 params liferange =
  let expire active free start =
    let rec loop rs free active =
      match active with
        [] -> (List.rev rs, free)
      | (reg_id, (a, b))::active' ->
         if b < start then
           loop rs (reg_id::free) active'
         else
           loop ((reg_id, (a, b))::rs) free active'
    in
    loop [] free active
  in
  let rec scan map active free intervals =
    (if !Option.debug then
       begin
         print_regmap map;
         print_active active;
         print_free free
       end);
    match intervals with
      [] -> map
    | (var, (a, b))::intervals' ->
       (if !Option.debug then
          printf "%s (%d, %d)\n--------------------\n" (Id.show var) a b);
       let (active, free) = expire active free a in
       (match free with
          [] -> error "lack of regs"
        | reg_id::free' ->
           let active = (reg_id, (a, b))::active in
           let map = IdMap.add var reg_id map in
           scan map active free' intervals')
  in
  (* params are active at the entry *)
  let active =
    List.mapi
      (fun i x ->
        let (a, b) = IdMap.find x liferange in
        (i, (a, b)))
      params
  in
  let free = interval (List.length params) num_registers in
  let intervals =
    List.sort
      (fun (_, (a1, _)) (_, (a2, _)) -> a1 - a2)
      (IdMap.fold
         (fun var (a, b) acc ->
           if List.mem var params then
             acc
           else
             (var, (a, b))::acc)
         liferange [])
  in
  (* params are pre-alloced *)
  let reg_map =
    List.fold_lefti
      (fun m i var -> IdMap.add var i m)
      IdMap.empty params
  in
  scan reg_map active free intervals

let reg_alloc params liferange =
  let regmap = reg_alloc1 params liferange in
  (if !Option.debug then
     IdMap.iter
       (fun var reg_id -> printf "%s: %d\n" (Id.show var) reg_id)
       regmap);
  regmap
