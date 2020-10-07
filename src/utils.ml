open Printf
open Error

let char_list_to_string cs =
  let n = List.length cs in
  let buf = Buffer.create n in
  List.iteri (fun i c -> Buffer.add_char buf c) cs;
  Buffer.contents buf

let string_to_char_list s =
  let rec f k cs =
    if k = 0 then
      cs
    else
      f (k-1) ((String.get s (k-1))::cs)
  in
  f (String.length s) []

(* path => (dir, fname, ext) *)
let splitpath path =
  let rec f rs = function
      [] -> ("", char_list_to_string rs, "")
    | c::cs ->
       if c = '.' then
         if rs = [] then
           g "" [] cs
         else
           g (char_list_to_string (c::rs)) [] cs
       else if c = '/' || c = '\\' || c = ':' then
         (char_list_to_string (List.rev (c::cs)), char_list_to_string rs, "")
       else
         f (c::rs) cs
  and g ext rs = function
      [] -> ("", char_list_to_string rs, ext)
    | c::cs ->
       if c = '/' || c = '\\' || c = ':' then
         (char_list_to_string (List.rev (c::cs)), char_list_to_string rs, ext)
       else
         g ext (c::rs) cs
  in
  f [] (List.rev (string_to_char_list path))

let rec interval a b =
  if a >= b then
    []
  else
    a::(interval (a+1) b)

let find_index v x =
  let n = Array.length v in
  let rec loop i =
    if i=n then
      error "find_index"
    else if v.(i) = x then
      i
    else
      loop (i+1)
  in loop 0

let take xs n =
  let rec f k xs rs =
    if k = n then
      List.rev rs
    else
      match xs with
        [] -> Error.error "take: out of range"
      | x::xs' -> f (k+1) xs' (x::rs)
  in
  if n < 0 then
    Error.error "take: out of range"
  else
    f 0 xs []

let rec drop xs n =
  let rec f xs n =
    if n = 0 then
      xs
    else
      match xs with
        [] -> Error.error "drop: out of range"
      | x::xs' -> drop xs' (n-1)
  in
  if n < 0 then
    Error.error "drop: out of range"
  else
    f xs n

let is_alpha c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
let is_digit c = c >= '0' && c <= '9'
let hex = "0123456789ABCDEF"

let c_name s =
  let n = String.length s in
  let buf = Buffer.create 0 in
  let rec loop k =
    if k = n then
      Bytes.to_string (Buffer.to_bytes buf)
    else
      let c = s.[k] in
      if is_alpha c || is_digit c then
        (Buffer.add_char buf c; loop (k+1))
      else
        let x = Char.code c in
        Buffer.add_char buf '_';
        Buffer.add_char buf hex.[x / 16];
        Buffer.add_char buf hex.[x mod 16];
        loop (k+1)
  in loop 0

let seq_count =
  let c = ref 0 in
  fun () -> let r = !c in c := r + 1; r

let alloc_label () = seq_count ()

let fold_map f acc xs =
  let rec loop acc rs xs =
    match xs with
      [] -> (acc, List.rev rs)
    | x::xs' ->
       let (acc, r) = f acc x in
       loop acc (r::rs) xs'
  in
  loop acc [] xs
