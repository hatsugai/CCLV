open Printf

exception Error of string

let error msg = raise (Error msg)
let error_s msg s = raise (Error (msg ^ s))
