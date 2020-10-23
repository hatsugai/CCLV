open Printf
open Utils
open Error
open Col
open IdCol
open S
open Ir

let noimp () = error "not implemented"

let label l = sprintf "label_%d" l

let codegen ch name params regmap inst_list =

  let num_params = List.length params in
  let max_var_idx = IdMap.fold (fun _ i m -> if i > m then i else m) regmap 0 in
  let num_locals = max_var_idx + 1 - num_params in

  let reg var =
    let i = IdMap.find var regmap in
    if i < num_params then
      i * 2 + num_locals * 2 + 4
    else
      (i - num_params) * 2
  in

  let lda_imm_l k =
    fprintf ch "lda a #%d\n" (k land 255)
  in

  let lda_imm_h k =
    fprintf ch "lda a #%d\n" ((k lsr 8) land 255)
  in

  let lda_l x =
    match x with
      Int k -> fprintf ch "lda a #%d\n" (k land 255)
    | Var v ->
       let r = reg v in
       fprintf ch "lda a %d,x\n" (r+1)
  in

  let ldb_l x =
    match x with
      Int k -> fprintf ch "lda b #%d\n" (k land 255)
    | Var v ->
       let r = reg v in
       fprintf ch "lda b %d,x\n" (r+1)
  in

  let lda_h x =
    match x with
      Int k -> fprintf ch "lda a #%d\n" ((k lsr 8) land 255)
    | Var v ->
       let r = reg v in
       fprintf ch "lda a %d,x\n" r
  in

  let sta_l x =
    let r = reg x in
    fprintf ch "sta a %d,x\n" (r+1)
  in

  let stb_l x =
    let r = reg x in
    fprintf ch "sta b %d,x\n" (r+1)
  in

  let sta_h x =
    let r = reg x in
    fprintf ch "sta a %d,x\n" r
  in

  let add_l x =
    match x with
      Int k -> fprintf ch "add a #%d\n" (k land 255)
    | Var v ->
       let r = reg v in
       fprintf ch "add a %d,x\n" (r+1)
  in

  let adc_h x =
    match x with
      Int k -> fprintf ch "adc a #%d\n" ((k lsr 8) land 255)
    | Var v ->
       let r = reg v in
       fprintf ch "adc a %d,x\n" r
  in

  let sub_l x =
    match x with
      Int k -> fprintf ch "sub a #%d\n" (k land 255)
    | Var v ->
       let r = reg v in
       fprintf ch "sub a %d,x\n" (r+1)
  in

  let sbc_h x =
    match x with
      Int k -> fprintf ch "sbc a #%d\n" ((k lsr 8) land 255)
    | Var v ->
       let r = reg v in
       fprintf ch "sbc a %d,x\n" r
  in

  let cmp_l x =
    match x with
      Int k -> fprintf ch "cmp a #%d\n" (k land 255)
    | Var v ->
       let r = reg v in
       fprintf ch "cmp a %d,x\n" (r+1)
  in

  let cmp_h x =
    match x with
      Int k -> fprintf ch "cmp a #%d\n" ((k lsr 8) land 255)
    | Var v ->
       let r = reg v in
       fprintf ch "cmp a %d,x\n" r
  in

  let and_l x =
    match x with
      Int k -> fprintf ch "and a #%d\n" (k land 255)
    | Var v ->
       let r = reg v in
       fprintf ch "and a %d,x\n" (r+1)
  in

  let and_h x =
    match x with
      Int k -> fprintf ch "and a #%d\n" ((k lsr 8) land 255)
    | Var v ->
       let r = reg v in
       fprintf ch "and a %d,x\n" r
  in

  let ora_l x =
    match x with
      Int k -> fprintf ch "ora a #%d\n" (k land 255)
    | Var v ->
       let r = reg v in
       fprintf ch "ora a %d,x\n" (r+1)
  in

  let ora_h x =
    match x with
      Int k -> fprintf ch "ora a #%d\n" ((k lsr 8) land 255)
    | Var v ->
       let r = reg v in
       fprintf ch "ora a %d,x\n" r
  in

  let eor_l x =
    match x with
      Int k -> fprintf ch "eor a #%d\n" (k land 255)
    | Var v ->
       let r = reg v in
       fprintf ch "eor a %d,x\n" (r+1)
  in

  let eor_h x =
    match x with
      Int k -> fprintf ch "eor a #%d\n" ((k lsr 8) land 255)
    | Var v ->
       let r = reg v in
       fprintf ch "eor a %d,x\n" r
  in

  let sub16 x y =
    lda_l x;
    sub_l y;
    lda_h x;
    sbc_h y
  in

  let codegen_assign x y =
    lda_l y;
    sta_l x;
    lda_h y;
    sta_h x
  in

  let codegen_load x y = () in
  let codegen_store x y = () in

  let codegen_call x f args =
    List.iteri
      (fun i y ->
        lda_l y;
        fprintf ch "psh a\n";
        lda_h y;
        fprintf ch "psh a\n")
      (List.rev args);
    fprintf ch "jsr %s\n" (Id.print_string f);
    List.iteri
      (fun i y ->
        fprintf ch "ins\n";
        fprintf ch "ins\n")
      args;
    if x <> Id.dummy then
      begin
        stb_l x;
        sta_h x
      end
  in

  let codegen_uniop x op y =
    let r = reg x in
    match y with
      Int k ->
       let k =
         match op with
         | Neg -> -k
         | Not -> if k = 0 then 1 else 0
         | BitNot -> lnot k
       in
       lda_imm_l k;
       sta_l x;
       lda_imm_h k;
       sta_h x
    | Var z ->
       let r2 = reg z in
       (match op with
        | Neg ->
           lda_imm_l 0;
           sub_l y;
           sta_l x;
           lda_imm_h 0;
           sbc_h y;
           sta_h x
        | Not ->
           let l1 = alloc_label () in
           let l2 = alloc_label () in
           fprintf ch "lda a %d,x\n" r2;
           fprintf ch "ora a %d,x\n" (r2+1);
           fprintf ch "bne %s\n" (label l1);
           fprintf ch "lda a #1\n";
           fprintf ch "sta a %d,x\n" r;
           fprintf ch "lda a #0\n";
           fprintf ch "sta a %d,x\n" (r+1);
           fprintf ch "bra %s\n" (label l2);
           fprintf ch "%s:\n" (label l1);
           fprintf ch "lda a #0\n";
           fprintf ch "sta a %d,x\n" r;
           fprintf ch "sta a %d,x\n" (r+1);
           fprintf ch "%s:\n" (label l2)
        | BitNot ->
           fprintf ch "lda a %d,x\n" r2;
           fprintf ch "com a\n";
           fprintf ch "sta a %d,x\n" r;
           fprintf ch "lda a %d,x\n" (r2+1);
           fprintf ch "com a\n";
           fprintf ch "sta a %d,x\n" (r+1))
  in

  let codegen_binop x op y z =
    match op with
    | Mul -> noimp ()
    | Div -> noimp ()
    | Mod -> noimp ()
    | Add ->
       lda_l y;
       add_l z;
       sta_l x;
       lda_h y;
       adc_h z;
       sta_h x
    | Sub ->
       lda_l y;
       sub_l z;
       sta_l x;
       lda_h y;
       sbc_h z;
       sta_h x
    | Lsh ->
       (match z with
          Int k ->
           (match y with
              Int m ->
               let n = m lsl k in
               lda_imm_l n;
               sta_l x;
               lda_imm_h n;
               sta_h x
            | Var v ->
               if k<8 then
                 begin
                   ldb_l y;
                   lda_h y;
                   for i=0 to k-1 do
                     fprintf ch "asl b\n";
                     fprintf ch "rol a\n"
                   done;
                   stb_l x;
                   sta_h x
                 end
               else if k=8 then
                 begin
                   fprintf ch "lda a #0\n";
                   sta_l x;
                   lda_l y;
                   sta_h x;
                 end
               else if k < 16 then
                 begin
                   fprintf ch "lda a #0\n";
                   sta_l x;
                   lda_l y;
                   for i=8 to k-1 do
                     fprintf ch "asl a\n"
                   done;
                   sta_h x
                 end
               else             (* nop *)
                 ())
        | Var v -> noimp ())
    | Rsh ->
       (match z with
          Int k ->
           (match y with
              Int m ->
               let n = m lsr k in
               lda_imm_l n;
               sta_l x;
               lda_imm_h n;
               sta_h x
            | Var v ->
               if k<8 then
                 begin
                   ldb_l y;
                   lda_h y;
                   for i=0 to k-1 do
                     fprintf ch "asr a\n";
                     fprintf ch "ror b\n";
                   done;
                   stb_l x;
                   sta_h x
                 end
               else if k=8 then
                 begin
                   fprintf ch "lda a #0\n";
                   sta_h x;
                   lda_h y;
                   sta_l x
                 end
               else if k<16 then
                 begin
                   fprintf ch "lda #0\n"; (* ### sign *)
                   sta_h x;
                   lda_h y;
                   for i=8 to k-1 do
                     fprintf ch "lsr a\n"
                   done;
                   sta_l x;
                 end
               else
                 ())
        | Var v -> noimp ())
    | Lt -> noimp ()
    | Le -> noimp ()
    | Eq -> noimp ()
    | BitAnd ->
       lda_l y;
       and_l z;
       sta_l x;
       lda_h y;
       and_h z;
       sta_h x
    | BitOr ->
       lda_l y;
       ora_l z;
       sta_l x;
       lda_h y;
       ora_h z;
       sta_h x
    | BitXor ->
       lda_l y;
       eor_l z;
       sta_l x;
       lda_h y;
       eor_h z;
       sta_h x
    | And -> noimp ()
    | Or -> noimp ()
  in

  let prologue () =
    (* 1. push x *)
    fprintf ch "stx WORK\n";
    fprintf ch "lda a WORK+1\n";
    fprintf ch "psh a\n";
    fprintf ch "lda a WORK\n";
    fprintf ch "psh a\n";
    (* 2. offset sp *)
    for i=0 to num_locals - 1 do
      fprintf ch "des\n";
      fprintf ch "des\n";
    done;
    (* X <- SP + 1 *)
    fprintf ch "tsx\n";
  in

  let epilogue () =
    (* 1. offset sp *)
    for i=0 to num_locals - 1 do
      fprintf ch "ins\n";
      fprintf ch "ins\n";
    done;
    fprintf ch "sta a WORKA\n"; (* save A *)
    fprintf ch "pul a\n";
    fprintf ch "sta a WORK\n";
    fprintf ch "pul a\n";
    fprintf ch "sta a WORK+1\n";
    fprintf ch "ldx WORK\n";
    fprintf ch "lda a WORKA\n"  (* restore A *)
  in

  let codegen inst =
    match inst with
    | Label k -> fprintf ch "%s:\n" (label k)
    | Assign (x, y) -> codegen_assign x y
    | Load (x, y) -> codegen_load x y
    | Store (x, y) -> codegen_store x y
    | Call (x, f, args) ->codegen_call x f args
    | Uniop (x, op, y) -> codegen_uniop x op y
    | Binop (x, op, y, z) -> codegen_binop x op y z
    | Bf (k, x) ->
       lda_l x;
       ora_h x;
       fprintf ch "beq %s\n" (label k)
    | Bt (k, x) ->
       lda_l x;
       ora_h x;
       fprintf ch "bne %s\n" (label k)
    | Bra k -> fprintf ch "jmp %s\n" (label k)
    | Beq (k, x, y) ->
       let l = alloc_label () in
       lda_l x;
       cmp_l y;
       fprintf ch "bne %s\n" (label l);
       lda_h x;
       cmp_h y;
       fprintf ch "bne %s\n" (label l);
       fprintf ch "bra %s\n" (label k);
       fprintf ch "%s:\n" (label l)
    | Bne (k, x, y) ->
       lda_l x;
       cmp_l y;
       fprintf ch "bne %s\n" (label k);
       lda_h x;
       cmp_h y;
       fprintf ch "bne %s\n" (label k)
    | Blt (k, x, y) ->          (* branch if x < y *)
       sub16 x y;
       fprintf ch "blt %s\n" (label k)
    | Ble (k, x, y) ->          (* branch if x <= y *)
       sub16 y x;
       fprintf ch "bge %s\n" (label k)
    | Bgt (k, x, y) ->          (* branch if x > y *)
       sub16 y x;
       fprintf ch "blt %s\n" (label k)
    | Bge (k, x, y) ->          (* branch if x >= y *)
       sub16 x y;
       fprintf ch "bge %s\n" (label k)
    | Return x ->
       ldb_l x;
       lda_h x;
       epilogue ();
       fprintf ch "rts\n"
  in

  let rec loop inst_list =
    match inst_list with
      [] -> ()
    | [inst] ->
       codegen inst;
       (match inst with
          Return _ -> ()
        | _ ->
           epilogue ();
           fprintf ch "rts\n")
    | inst::inst_list' ->
       codegen inst;
       loop inst_list'
  in

  fprintf ch "%s:\n" (Id.print_string name);
  prologue ();
  loop inst_list

let preamble ch =
  fprintf ch "WORKA = 0xFD\n";
  fprintf ch "WORK = 0xFE\n";
  fprintf ch ".area _CODE\n";
  fprintf ch ".globl main,getch,putch\n"
