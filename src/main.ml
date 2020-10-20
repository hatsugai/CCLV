open Printf
open Col
open IdCol
open S
open Ir
open Compile
open Regalloc

type machine =
  Fenestra6502
| MC6800

let machine = ref Fenestra6502

let usage_msg = "cclv <option>* <source>\n"

let select_machine s =
  if s="Fenestra6502" then
    machine := Fenestra6502
  else if s="6800" then
    machine := MC6800
  else
    begin
      fprintf stderr "unknown machine: %s\n" s;
      exit 1
    end

let option_spec =
  [
    ("-m", Arg.String select_machine, "");
    ("--debug", Arg.Unit (fun () -> Option.debug := true), "");
  ]

let g_args : string list ref = ref []

let anon_fun s = g_args := !g_args @ [s]

let parse path =
  let ch = open_in path in
  let lexbuf = Lexing.from_channel ch in
  let fun_def_list_rev = Parser.translation_unit Lexer.token lexbuf in
  close_in ch;
  let fun_def_list = List.rev fun_def_list_rev in
  (if !Option.debug then
     List.iter
       (fun f ->
         Printf.printf "%s: %s\n" (Id.show f.name) (show_statement f.body))
       fun_def_list);
  fun_def_list

let emit_prologue ch fun_def_list =
  match !machine with
    Fenestra6502 ->
     fprintf ch ".r65c02\n";
     fprintf ch ".area _CODE\n";
     fprintf ch ".globl main,getch,putch\n"
  | MC6800 ->
     fprintf ch "WORKA = 0xFD\n";
     fprintf ch "WORK = 0xFE\n";
     fprintf ch ".area _CODE\n";
     fprintf ch ".globl main,getch,putch\n"

let codegen ch name params regmap inst_list =
  match !machine with
    Fenestra6502 ->
     CodegenFenestra.codegen ch name params regmap inst_list
  | MC6800 ->
     Codegen6800.codegen ch name params regmap inst_list

let main () =
  Arg.parse option_spec anon_fun usage_msg;
  match !g_args with
    [src_path] ->
     let (dir, fname, ext) = Utils.splitpath src_path in
     let asm_path = fname ^ ".asm" in (* cwd *)
     let fun_def_list = parse src_path in
     let ch = open_out asm_path in
     emit_prologue ch fun_def_list;
     List.iter
       (fun fun_def ->
         let stmt = Norm.normalize fun_def in
         let is = compile cenv0 stmt [] in
         let liferange = Regalloc.calc_life_range fun_def.parameter_list is in
         let regmap = reg_alloc fun_def.parameter_list liferange in
         codegen ch fun_def.name fun_def.parameter_list regmap is)
       fun_def_list
  | _ ->
     Arg.usage option_spec usage_msg

let () = main ()
