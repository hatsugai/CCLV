open Printf
open Utils
open Error
open Col
open IdCol
open S
open Ir

let noimp () = error "not implemented"

let label l = sprintf "label_%d" l

let get_max_num_of_args inst_list =
  let rec loop m inst_list =
    match inst_list with
      [] -> m
    | inst::inst_list' ->
       (match inst with
          Call (_, _, args) ->
           let n = List.length args in
           loop (if n > m then n else m) inst_list'
        | _ -> loop m inst_list')
  in
  loop 0 inst_list

let codegen ch name params regmap inst_list =

  let max_args = get_max_num_of_args inst_list in
  let num_params = List.length params in
  let max_var_idx = IdMap.fold (fun _ i m -> if i > m then i else m) regmap 0 in
  let num_locals = max_var_idx + 1 - num_params in

  let reg var =
    let i = IdMap.find var regmap in
    if i < num_params then
      (max_args + num_locals + i) * 2
    else
      (i - num_params + max_args) * 2
  in

  (* N = 0 iff x >= y *)
  let cmp16 x y =
    let l = alloc_label () in
    match x, y with
    | Int a, Int b ->          (* ### optimize *)
       if a >= b then
         fprintf ch "lda #%d\n" 0
       else
         fprintf ch "lda #%d\n" 0xFF
    | Int k, Var v ->
       let r = reg v in
       fprintf ch "ldy #%d\n" r;
       fprintf ch "lda #%d\n" (k land 255);
       fprintf ch "cmp [SP],y\n";
       fprintf ch "lda #%d\n" ((k lsr 8) land 255);
       fprintf ch "iny\n";
       fprintf ch "sbc [SP],y\n";
	   fprintf ch "bvc %s\n" (label l);
       fprintf ch "eor #0x80\n";
       fprintf ch "%s:\n" (label l)
    | Var v, Int k ->
       let r = reg v in
       fprintf ch "ldy #%d\n" r;
       fprintf ch "lda [SP],y\n";
       fprintf ch "cmp #%d\n" (k land 255);
       fprintf ch "iny\n";
       fprintf ch "lda [SP],y\n";
       fprintf ch "sbc #%d\n" ((k lsr 8) land 255);
	   fprintf ch "bvc %s\n" (label l);
       fprintf ch "eor #0x80\n";
       fprintf ch "%s:\n" (label l)
    | Var v1, Var v2 ->
       let r1 = reg v1 in
       let r2 = reg v2 in
       fprintf ch "ldy #%d\n" r1;
       fprintf ch "lda [SP],y\n";
       fprintf ch "ldy #%d\n" r2;
       fprintf ch "cmp [SP],y\n";
       fprintf ch "ldy #%d\n" (r1+1);
       fprintf ch "lda [SP],y\n";
       fprintf ch "ldy #%d\n" (r2+1);
       fprintf ch "sbc [SP],y\n";
	   fprintf ch "bvc %s\n" (label l);
       fprintf ch "eor #0x80\n";
       fprintf ch "%s:\n" (label l)
  in

  let codegen_assign x y =
    match y with
    | Int k ->
       let r = reg x in
       fprintf ch "ldy #%d\n" r;
       fprintf ch "lda #%d\n" (k land 255);
       fprintf ch "sta [SP],y\n";
       fprintf ch "lda #%d\n" ((k lsr 8) land 255);
       fprintf ch "iny\n";
       fprintf ch "sta [SP],y\n"
    | Var v ->
       let r1 = reg x in
       let r2 = reg v in
       fprintf ch "ldy #%d\n" r2;
       fprintf ch "lda [SP],y\n";
       fprintf ch "ldy #%d\n" r1;
       fprintf ch "sta [SP],y\n";
       fprintf ch "ldy #%d\n" (r2+1);
       fprintf ch "lda [SP],y\n";
       fprintf ch "ldy #%d\n" (r1+1);
       fprintf ch "sta [SP],y\n"
  in

  let codegen_load x y = () in
  let codegen_store x y = () in

  let codegen_call x f args =
    List.iteri
      (fun i y ->
        match y with
        | Int k ->
           fprintf ch "ldy #%d\n" (i * 2);
           fprintf ch "lda #%d\n" (k land 255);
           fprintf ch "sta [SP],y\n";
           fprintf ch "iny\n";
           fprintf ch "lda #%d\n" ((k lsr 8) land 255);
           fprintf ch "sta [SP],y\n"
        | Var v ->
           let r = reg v in
           fprintf ch "ldy #%d\n" r;
           fprintf ch "lda [SP],y\n";
           fprintf ch "ldy #%d\n" (i * 2);
           fprintf ch "sta [SP],y\n";
           fprintf ch "ldy #%d\n" (r + 1);
           fprintf ch "lda [SP],y\n";
           fprintf ch "ldy #%d\n" (i * 2 + 1);
           fprintf ch "sta [SP],y\n")
      args;
    fprintf ch "jsr %s\n" (Id.print_string f);
    (if x <> Id.dummy then
       let r = reg x in
       fprintf ch "ldy #%d\n" (r+1);
       fprintf ch "sta [SP],y\n";
       fprintf ch "txa\n";      (* low *)
       fprintf ch "dey\n";
       fprintf ch "sta [SP],y\n")
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
       fprintf ch "ldy #%d\n" r;
       fprintf ch "lda #%d\n" (k land 255);
       fprintf ch "sta [SP],y\n";
       fprintf ch "lda #%d\n" ((k lsr 8) land 255);
       fprintf ch "iny\n";
       fprintf ch "sta [SP],y"
    | Var z ->
       let r2 = reg z in
       (match op with
        | Neg ->
           fprintf ch "sec\n";
           fprintf ch "lda #0\n";
           fprintf ch "ldy #%d\n" r2;
           fprintf ch "sbc [SP],y\n";
           fprintf ch "ldy #%d\n" r;
           fprintf ch "sta [SP],y\n";
           fprintf ch "lda #0\n";
           fprintf ch "ldy #%d\n" (r2+1);
           fprintf ch "sbc [SP],y\n";
           fprintf ch "ldy #%d\n" (r+1);
           fprintf ch "sta [SP],y\n";
        | Not ->
           let l1 = alloc_label () in
           let l2 = alloc_label () in
           fprintf ch "ldy #%d\n" r2;
           fprintf ch "lda [SP],y\n";
           fprintf ch "iny\n";
           fprintf ch "ora [SP],y\n";
           fprintf ch "bne %s\n" (label l1);
           (* y=0 *)
           fprintf ch "ldy #%d\n" r;
           fprintf ch "lda #1\n";
           fprintf ch "sta [SP],y\n";
           fprintf ch "lda #0\n";
           fprintf ch "iny\n";
           fprintf ch "sta [SP],y\n";
           fprintf ch "bra %s\n" (label l2);
           (* y<>0 *)
           fprintf ch "%s:\n" (label l1);
           fprintf ch "ldy #%d\n" r;
           fprintf ch "lda #0\n";
           fprintf ch "sta [SP],y\n";
           fprintf ch "iny\n";
           fprintf ch "sta [SP],y\n";
           fprintf ch "%s:\n" (label l2)
        | BitNot ->
           fprintf ch "ldy #%d\n" r2;
           fprintf ch "lda [SP],y\n";
           fprintf ch "eor #0xFF\n";
           fprintf ch "ldy #%d\n" r;
           fprintf ch "sta [SP],y\n";
           fprintf ch "ldy #%d\n" (r2+1);
           fprintf ch "lda [SP],y\n";
           fprintf ch "eor #0xFF\n";
           fprintf ch "ldy #%d\n" (r+1);
           fprintf ch "sta [SP],y\n")
  in

  let codegen_binop_help x op y z pre const_fun =
    let r = reg x in
    match y, z with
    | Int a, Int b ->          (* ### optimize *)
       let c = const_fun a b in
       fprintf ch "ldy #%d\n" r;
       fprintf ch "lda #%d\n" (c land 255);
       fprintf ch "sta [SP],y\n";
       fprintf ch "iny\n";
       fprintf ch "lda #%d\n" ((c lsr 8) land 255);
       fprintf ch "sta [SP],y\n"
    | Int k, Var v ->
       let r2 = reg v in
       (match pre with None -> () | Some s -> fprintf ch "%s\n" s);
       fprintf ch "ldy #%d\n" r2;
       fprintf ch "lda #%d\n" (k land 255);
       fprintf ch "%s [SP],y\n" op;
       fprintf ch "ldy #%d\n" r;
       fprintf ch "sta [SP],y\n";
       fprintf ch "lda #%d\n" ((k lsr 8) land 255);
       fprintf ch "ldy #%d\n" (r2+1);
       fprintf ch "%s [SP],y\n" op;
       fprintf ch "ldy #%d\n" (r+1);
       fprintf ch "sta [SP],y\n"
    | Var v, Int k ->
       let r2 = reg v in
       (match pre with None -> () | Some s -> fprintf ch "%s\n" s);
       fprintf ch "ldy #%d\n" r2;
       fprintf ch "lda [SP],y\n";
       fprintf ch "%s #%d\n" op (k land 255);
       fprintf ch "ldy #%d\n" r;
       fprintf ch "sta [SP],y\n";
       fprintf ch "ldy #%d\n" (r2+1);
       fprintf ch "lda [SP],y\n";
       fprintf ch "%s #%d\n" op ((k lsr 8) land 255);
       fprintf ch "ldy #%d\n" (r+1);
       fprintf ch "sta [SP],y\n"
    | Var v1, Var v2 ->
       let r1 = reg v1 in
       let r2 = reg v2 in
       (match pre with None -> () | Some s -> fprintf ch "%s\n" s);
       fprintf ch "ldy #%d\n" r1;
       fprintf ch "lda [SP],y\n";
       fprintf ch "ldy #%d\n" r2;
       fprintf ch "%s [SP],y\n" op;
       fprintf ch "ldy #%d\n" r;
       fprintf ch "sta [SP],y\n";
       fprintf ch "ldy #%d\n" (r1+1);
       fprintf ch "lda [SP],y\n";
       fprintf ch "ldy #%d\n" (r2+1);
       fprintf ch "%s [SP],y\n" op;
       fprintf ch "ldy #%d\n" (r+1);
       fprintf ch "sta [SP],y\n"
  in

  let codegen_binop x op y z =
    let r = reg x in
    match op with
    | Mul -> noimp ()
    | Div -> noimp ()
    | Mod -> noimp ()
    | Add ->
       codegen_binop_help x "adc" y z (Some "clc") (fun a b -> a + b)
    | Sub ->
       codegen_binop_help x "sbc" y z (Some "sec") (fun a b -> a - b)
    | BitAnd ->
       codegen_binop_help x "and" y z None (fun a b -> a - b)
    | BitOr ->
       codegen_binop_help x "ora" y z None (fun a b -> a - b)
    | BitXor ->
       codegen_binop_help x "eor" y z None (fun a b -> a - b)
    | Lsh ->
       (match y, z with
        | Int m, Int k ->
           let n = m lsl k in
           fprintf ch "lda #%d\n" (n land 255);
           fprintf ch "ldy #%d\n" r;
           fprintf ch "sta [SP],y\n";
           fprintf ch "lda #%d\n" ((n lsr 8) land 255);
           fprintf ch "iny\n";
           fprintf ch "sta [SP],y\n"
        | Var v, Int k ->
           let r2 = reg v in
           if k<8 then
             begin
               fprintf ch "ldy #%d\n" r2;
               fprintf ch "lda [SP],y\n";
               fprintf ch "sta *WORK\n";
               fprintf ch "iny\n";
               fprintf ch "lda [SP],y\n"; (* a:high WORK:low *)
               let r = reg x in
               for i=0 to k-1 do
                 fprintf ch "asl *WORK\n";
                 fprintf ch "rol a\n"
               done;
               fprintf ch "ldy #%d\n" (r+1);
               fprintf ch "sta [SP],y\n";
               fprintf ch "lda *WORK\n";
               fprintf ch "dey\n";
               fprintf ch "sta [SP],y\n";
             end
           else if k=8 then
             begin
               fprintf ch "ldy #%d\n" r2;
               fprintf ch "lda [SP],y\n";
               fprintf ch "ldy #%d\n" (r+1);
               fprintf ch "sta [SP],y\n";
               fprintf ch "lda #0\n";
               fprintf ch "dey\n";
               fprintf ch "sta [SP],y\n";
             end
           else if k < 16 then
             begin
               fprintf ch "ldy #%d\n" r2;
               fprintf ch "lda [SP],y\n";
               for i=8 to k-1 do
                 fprintf ch "asl a\n"
               done;
               fprintf ch "ldy #%d\n" (r+1);
               fprintf ch "sta [SP],y\n";
               fprintf ch "lda #0\n";
               fprintf ch "dey\n";
               fprintf ch "sta [SP],y\n"
             end
           else             (* nop *)
             ()
        | _, Var v -> noimp ())
    | Rsh ->
       (match y, z with
        | Int m, Int k ->
           let n = m asr k in
           fprintf ch "lda #%d\n" (n land 255);
           fprintf ch "ldy #%d\n" r;
           fprintf ch "sta [SP],y\n";
           fprintf ch "lda #%d\n" ((n lsr 8) land 255);
           fprintf ch "iny\n";
           fprintf ch "sta [SP],y\n"
        | Var v, Int k ->
           let r2 = reg v in
           if k<8 then
             begin
               fprintf ch "ldy #%d\n" r2;
               fprintf ch "lda [SP],y\n";
               fprintf ch "sta *WORK\n";
               fprintf ch "iny\n";
               fprintf ch "lda [SP],y\n"; (* a:high WORK:low *)
               let r = reg x in
               for i=0 to k-1 do
                 fprintf ch "lsr a\n"; (* ### sign *)
                 fprintf ch "ror *WORK\n"
               done;
               fprintf ch "ldy #%d\n" (r+1);
               fprintf ch "sta [SP],y\n";
               fprintf ch "lda *WORK\n";
               fprintf ch "dey\n";
               fprintf ch "sta [SP],y\n";
             end
           else if k=8 then
             begin
               fprintf ch "ldy #%d\n" (r2+1);
               fprintf ch "lda [SP],y\n";
               fprintf ch "ldy #%d\n" r;
               fprintf ch "sta [SP],y\n";
               fprintf ch "lda #0\n";
               fprintf ch "iny\n";
               fprintf ch "sta [SP],y\n";
             end
           else if k < 16 then
             begin              (* ### sign *)
               fprintf ch "ldy #%d\n" (r2+1);
               fprintf ch "lda [SP],y\n";
               for i=8 to k-1 do
                 fprintf ch "asl a\n"
               done;
               fprintf ch "ldy #%d\n" r;
               fprintf ch "sta [SP],y\n";
               fprintf ch "lda #0\n";
               fprintf ch "iny\n";
               fprintf ch "sta [SP],y\n"
             end
           else
             ()
        | _, Var v -> noimp ())
    | Lt -> noimp ()
    | Le -> noimp ()
    | Eq -> noimp ()
    | And -> noimp ()
    | Or -> noimp ()
  in

  let prologue () =
    if max_args + num_locals > 0 then
      begin
        fprintf ch "sec\n";
        fprintf ch "lda *SP\n";
        fprintf ch "sbc #%d\n" ((max_args + num_locals) * 2);
        fprintf ch "sta *SP\n";
        fprintf ch "lda *(SP+1)\n";
        fprintf ch "sbc #0\n";
        fprintf ch "sta *(SP+1)\n";
      end
  in

  let epilogue () =
    if max_args + num_locals > 0 then
      begin
        fprintf ch "sta *WORK\n";
        fprintf ch "clc\n";
        fprintf ch "lda *SP\n";
        fprintf ch "adc #%d\n" ((max_args + num_locals) * 2);
        fprintf ch "sta *SP\n";
        fprintf ch "lda *(SP+1)\n";
        fprintf ch "adc #0\n";
        fprintf ch "sta *(SP+1)\n";
        fprintf ch "lda *WORK\n"
      end
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
       (match x with
        | Int k ->
           if k=0 then
             fprintf ch "bra %s\n" (label k)
        | Var v ->
           let r = reg v in
           fprintf ch "ldy #%d\n" r;
           fprintf ch "lda [SP],y\n";
           fprintf ch "iny\n";
           fprintf ch "ora [SP],y\n";
           fprintf ch "beq %s\n" (label k))
    | Bt (k, x) ->
       (match x with
        | Int k ->
           if k<>0 then
             fprintf ch "bra %s\n" (label k)
        | Var v ->
           let r = reg v in
           fprintf ch "ldy #%d\n" r;
           fprintf ch "lda [SP],y\n";
           fprintf ch "iny\n";
           fprintf ch "ora [SP],y\n";
           fprintf ch "bne %s\n" (label k))
    | Bra k ->
       fprintf ch "jmp %s\n" (label k)
    | Beq (k, x, y) ->
       let l = alloc_label () in
       let beq_helper v c =
         let r = reg v in
         fprintf ch "ldy #%d\n" r;
         fprintf ch "lda [SP],y\n";
         fprintf ch "cmp #%d\n" (c land 255);
         fprintf ch "bne %s\n" (label l);
         fprintf ch "iny\n";
         fprintf ch "lda [SP],y\n";
         fprintf ch "cmp #%d\n" ((c lsr 8) land 255);
         fprintf ch "bne %s\n" (label l);
         fprintf ch "bra %s\n" (label k);
         fprintf ch "%s:\n" (label l)
       in
       (match x, y with
        | Int a, Int b ->
           if a=b then
             fprintf ch "bra %s\n" (label k)
        | Int c, Var v ->
           beq_helper v c
        | Var v, Int c ->
           beq_helper v c
        | Var v1, Var v2 ->
           let r1 = reg v1 in
           let r2 = reg v2 in
           fprintf ch "ldy #%d\n" r1;
           fprintf ch "lda [SP],y\n";
           fprintf ch "ldy #%d\n" r2;
           fprintf ch "cmp [SP],y\n";
           fprintf ch "bne %s\n" (label l);
           fprintf ch "ldy #%d\n" (r1+1);
           fprintf ch "lda [SP],y\n";
           fprintf ch "ldy #%d\n" (r2+1);
           fprintf ch "cmp [SP],y\n";
           fprintf ch "bne %s\n" (label l);
           fprintf ch "bra %s\n" (label k);
           fprintf ch "%s:\n" (label l))
    | Bne (k, x, y) ->
       let beq_helper v c =
         let r = reg v in
         fprintf ch "ldy #%d\n" r;
         fprintf ch "lda [SP],y\n";
         fprintf ch "cmp #%d\n" (c land 255);
         fprintf ch "bne %s\n" (label k);
         fprintf ch "iny\n";
         fprintf ch "lda [SP],y\n";
         fprintf ch "cmp #%d\n" ((c lsr 8) land 255);
         fprintf ch "bne %s\n" (label k)
       in
       (match x, y with
        | Int a, Int b ->
           if a<>b then
             fprintf ch "bra %s\n" (label k)
        | Int c, Var v ->
           beq_helper v c
        | Var v, Int c ->
           beq_helper v c
        | Var v1, Var v2 ->
           let r1 = reg v1 in
           let r2 = reg v2 in
           fprintf ch "ldy #%d\n" r1;
           fprintf ch "lda [SP],y\n";
           fprintf ch "ldy #%d\n" r2;
           fprintf ch "cmp [SP],y\n";
           fprintf ch "bne %s\n" (label k);
           fprintf ch "ldy #%d\n" (r1+1);
           fprintf ch "lda [SP],y\n";
           fprintf ch "ldy #%d\n" (r2+1);
           fprintf ch "cmp [SP],y\n";
           fprintf ch "bne %s\n" (label k))
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
       (match x with
        | Int k ->
           fprintf ch "ldx #%d\n" (k land 255);
           fprintf ch "lda #%d\n" ((k lsr 8) land 255);
           epilogue ();
           fprintf ch "rts\n"
        | Var v ->
           let r = reg v in
           fprintf ch "ldy #%d\n" r;
           fprintf ch "lda [SP],y\n";
           fprintf ch "tax\n";  (* x:low *)
           fprintf ch "iny\n";
           fprintf ch "lda [SP],y\n"; (* a:high *)
           epilogue ();
           fprintf ch "rts\n")
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
  fprintf ch "WORK=0xFD\n";
  fprintf ch "SP=0xFE\n";
  fprintf ch ".r65c02\n";
  fprintf ch ".area _CODE\n";
  fprintf ch ".globl main,getch,putch\n"
