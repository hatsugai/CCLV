type uniop =
  | Neg
  | Not
  | BitNot
[@@deriving show]

type binop =
  | Mul
  | Div
  | Mod
  | Add
  | Sub
  | Lsh
  | Rsh
  | Lt
  | Le
  | Eq
  | BitAnd
  | BitOr
  | BitXor
  | And
  | Or
[@@deriving show]

type expr =
  | Int of int
  | Var of Id.t
  | Ref of expr
  | Call of Id.t * expr list
  | Uniop of uniop * expr
  | Binop of binop * expr * expr
[@@deriving show]

type statement =
| Skip
| Assign of Id.t * expr
| Store of expr * expr
| If of expr * statement * statement
| While of expr * statement
| WhileS of statement * expr * statement
| Compound of statement list
| Return of expr
[@@deriving show]

type function_definition = {
    name : Id.t;
    parameter_list : Id.t list;
    body : statement;
}

