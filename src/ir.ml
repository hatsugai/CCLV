type operand = Var of Id.t | Int of int
[@@deriving show]

type t =
| Label of int
| Assign of Id.t * operand
| Load of Id.t * operand        (* x = *p *)
| Store of operand * operand    (* *p = x *)
| Call of Id.t * Id.t * operand list
| Uniop of Id.t * S.uniop * operand           (* x := (op) e *)
| Binop of Id.t * S.binop * operand * operand (* x := e1 (op) e2 *)
| Bf of int * operand
| Bt of int * operand
| Bra of int
| Beq of int * operand * operand
| Bne of int * operand * operand
| Blt of int * operand * operand
| Ble of int * operand * operand
| Bgt of int * operand * operand
| Bge of int * operand * operand
| Return of operand
[@@deriving show]
