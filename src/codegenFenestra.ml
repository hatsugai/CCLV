open Printf
open Utils
open Error
open Col
open IdCol
open S
open Ir

let reg_offset = 128

let noimp () = error "not implemented"

let label l = sprintf "label_%d" l

let codegen ch name params regmap inst_list =

  let reg var = 2 * (IdMap.find var regmap) in

  let sta_l x =
    let r = reg x in
    fprintf ch "sta *%d\n" r
  in

  let sta_h x =
    let r = reg x in
    fprintf ch "sta *%d\n" (r+1)
  in

  let sta_pl i = fprintf ch "sta *%d\n" (reg_offset + i * 2) in
  let sta_ph i = fprintf ch "sta *%d\n" (reg_offset + i * 2 + 1) in

  let lda_l x =
    match x with
      Int k -> fprintf ch "lda #%d\n" (k land 255)
    | Var v ->
       let r = reg v in
       fprintf ch "lda *%d\n" r
  in

  let lda_h x =
    match x with
      Int k -> fprintf ch "lda #%d\n" ((k lsr 8) land 255)
    | Var v ->
       let r = reg v in
       fprintf ch "lda *%d\n" (r+1)
  in

  (* let ldx_l x =
   *   match x with
   *     Int k -> fprintf ch "ldx #%d\n" (k land 255)
   *   | Var v ->
   *      let r = reg v in
   *      fprintf ch "ldx *%d\n" r
   * in *)

  let ldx_h x =
    match x with
      Int k -> fprintf ch "ldx #%d\n" ((k lsr 8) land 255)
    | Var v ->
       let r = reg v in
       fprintf ch "ldx *%d\n" (r+1)
  in

  let adc_l x =
    match x with
      Int k -> fprintf ch "adc #%d\n" (k land 255)
    | Var v ->
       let r = reg v in
       fprintf ch "adc *%d\n" r
  in

  let adc_h x =
    match x with
      Int k -> fprintf ch "adc #%d\n" ((k lsr 8) land 255)
    | Var v ->
       let r = reg v in
       fprintf ch "adc *%d\n" (r+1)
  in

  let sbc_l x =
    match x with
      Int k -> fprintf ch "sbc #%d\n" (k land 255)
    | Var v ->
       let r = reg v in
       fprintf ch "sbc *%d\n" r
  in

  let sbc_h x =
    match x with
      Int k -> fprintf ch "sbc #%d\n" ((k lsr 8) land 255)
    | Var v ->
       let r = reg v in
       fprintf ch "sbc *%d\n" (r+1)
  in
  let cmp_l x =
    match x with
      Int k -> fprintf ch "cmp #%d\n" (k land 255)
    | Var v ->
       let r = reg v in
       fprintf ch "cmp *%d\n" r
  in

  let cmp_h x =
    match x with
      Int k -> fprintf ch "cmp #%d\n" ((k lsr 8) land 255)
    | Var v ->
       let r = reg v in
       fprintf ch "cmp *%d\n" (r+1)
  in

  let and_l x =
    match x with
      Int k -> fprintf ch "and #%d\n" (k land 255)
    | Var v ->
       let r = reg v in
       fprintf ch "and *%d\n" r
  in

  let and_h x =
    match x with
      Int k -> fprintf ch "and #%d\n" ((k lsr 8) land 255)
    | Var v ->
       let r = reg v in
       fprintf ch "and *%d\n" (r+1)
  in

  let ora_l x =
    match x with
      Int k -> fprintf ch "ora #%d\n" (k land 255)
    | Var v ->
       let r = reg v in
       fprintf ch "ora *%d\n" r
  in

  let ora_h x =
    match x with
      Int k -> fprintf ch "ora #%d\n" ((k lsr 8) land 255)
    | Var v ->
       let r = reg v in
       fprintf ch "ora *%d\n" (r+1)
  in

  let eor_l x =
    match x with
      Int k -> fprintf ch "eor #%d\n" (k land 255)
    | Var v ->
       let r = reg v in
       fprintf ch "eor *%d\n" r
  in

  let eor_h x =
    match x with
      Int k -> fprintf ch "eor #%d\n" ((k lsr 8) land 255)
    | Var v ->
       let r = reg v in
       fprintf ch "eor *%d\n" (r+1)
  in

  (* N = 0 iff x >= y *)
  let cmp16 x y =
    let l = alloc_label () in
    lda_l x;
    cmp_l y;
    lda_h x;
    sbc_h y;
    fprintf ch "bvc %s\n" (label l);
    fprintf ch "eor #0x80\n";
    fprintf ch "%s:\n" (label l)
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
        sta_pl i;
        lda_h y;
        sta_ph i)
      args;
    fprintf ch "jsr %s\n" (Id.print_string f);
    if x <> Id.dummy then
      let r = reg x in
      fprintf ch "sta *%d\n" r;
      fprintf ch "stx *%d\n" (r+1)
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
       fprintf ch "lda #%d\n" (k land 255);
       fprintf ch "sta *%d\n" r;
       fprintf ch "lda #%d\n" ((k lsr 8) land 255);
       fprintf ch "sta *%d\n" (r+1)
    | Var z ->
       let r2 = reg z in
       (match op with
        | Neg ->
           fprintf ch "sec\n";
           fprintf ch "lda #0\n";
           fprintf ch "sbc *%d\n" r2;
           fprintf ch "sta *%d\n" r;
           fprintf ch "lda #0\n";
           fprintf ch "sbc *%d\n" (r2+1);
           fprintf ch "sta *%d\n" (r+1)
        | Not ->
           let l1 = alloc_label () in
           let l2 = alloc_label () in
           fprintf ch "lda *%d\n" r2;
           fprintf ch "ora *%d\n" (r2+1);
           fprintf ch "bne %s\n" (label l1);
           fprintf ch "lda #1\n";
           fprintf ch "sta *%d\n" r;
           fprintf ch "lda #0\n";
           fprintf ch "sta *%d\n" (r+1);
           fprintf ch "bra %s\n" (label l2);
           fprintf ch "%s:\n" (label l1);
           fprintf ch "lda #0\n";
           fprintf ch "sta *%d\n" r;
           fprintf ch "sta *%d\n" (r+1);
           fprintf ch "%s:\n" (label l2)
        | BitNot ->
           fprintf ch "lda *%d\n" r2;
           fprintf ch "eor #0xFF\n";
           fprintf ch "sta *%d\n" r;
           fprintf ch "lda *%d\n" (r2+1);
           fprintf ch "eor #0xFF\n";
           fprintf ch "sta *%d\n" (r+1))
  in

  let codegen_binop x op y z =
    match op with
    | Mul -> noimp ()
    | Div -> noimp ()
    | Mod -> noimp ()
    | Add ->
       fprintf ch "clc\n";
       lda_l y;
       adc_l z;
       sta_l x;
       lda_h y;
       adc_h z;
       sta_h x
    | Sub ->
       fprintf ch "sec\n";
       lda_l y;
       sbc_l z;
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
               fprintf ch "lda #%d\n" (n land 255);
               sta_l x;
               fprintf ch "lda #%d\n" ((n lsr 8) land 255);
               sta_h x
            | Var v ->
               if k<8 then
                 begin
                   lda_l y;
                   sta_l x;
                   lda_h y;
                   sta_h x;
                   let r = reg x in
                   for i=0 to k-1 do
                     fprintf ch "asl *%d\n" r;
                     fprintf ch "rol *%d\n" (r+1);
                   done
                 end
               else if k=8 then
                 begin
                   fprintf ch "lda #0\n";
                   sta_l x;
                   lda_l y;
                   sta_h x;
                 end
               else if k < 16 then
                 begin
                   fprintf ch "lda #0\n";
                   sta_l x;
                   lda_l y;
                   sta_h x;
                   let r = reg x in
                   for i=8 to k-1 do
                     fprintf ch "asl *%d\n" (r+1)
                   done
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
               fprintf ch "lda #%d\n" (n land 255);
               sta_l x;
               fprintf ch "lda #%d\n" ((n lsr 8) land 255);
               sta_h x
            | Var v ->
               if k<8 then
                 begin
                   lda_l y;
                   sta_l x;
                   lda_h y;
                   sta_h x;
                   let r = reg x in
                   for i=0 to k-1 do
                     fprintf ch "lsr *%d\n" (r+1);
                     fprintf ch "ror *%d\n" r;
                   done
                 end
               else if k=8 then
                 begin
                   fprintf ch "lda #0\n";
                   sta_h x;
                   lda_h y;
                   sta_l x
                 end
               else if k<16 then
                 begin
                   fprintf ch "lda #0\n";
                   sta_h x;
                   lda_h y;
                   sta_l x;
                   let r = reg x in
                   for i=8 to k-1 do
                     fprintf ch "lsr *%d\n" r
                   done
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
       cmp16 x y;
       fprintf ch "bmi %s\n" (label k)
    | Ble (k, x, y) ->          (* branch if x <= y *)
       cmp16 y x;
       fprintf ch "bpl %s\n" (label k)
    | Bgt (k, x, y) ->          (* branch if x > y *)
       cmp16 y x;
       fprintf ch "bmi %s\n" (label k)
    | Bge (k, x, y) ->          (* branch if x >= y *)
       cmp16 x y;
       fprintf ch "bpl %s\n" (label k)
    | Return x ->
       lda_l x;
       ldx_h x;
       fprintf ch "rts\n"
  in

  let rec loop inst_list =
    match inst_list with
      [] -> ()
    | [inst] ->
       codegen inst;
       (match inst with
          Return _ -> ()
        | _ -> fprintf ch "rts\n")
    | inst::inst_list' ->
       codegen inst;
       loop inst_list'
  in

  fprintf ch "%s:\n" (Id.print_string name);
  loop inst_list

let preamble ch =
  fprintf ch ".r65c02\n";
  fprintf ch ".area _CODE\n";
  fprintf ch ".globl main,getch,putch\n"
