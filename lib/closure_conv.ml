(* Closure conversion *)

open Ast_typed

(* [union a b]: the variables of [a], then those of [b] not already in [a].
   Variables are compared by name only. The order is kept, because it
   becomes the layout of the closure's environment. *)
let union a b =
  a @ List.filter (fun (y, _) -> not (List.mem_assoc y a)) b

(* [remove x vs]: [vs] without the variable named [x]. *)
let remove x vs = List.filter (fun (y, _) -> y <> x) vs

(* [free_vars e]: the variables used in [e] but not bound inside it,
   each with its type, without duplicates, in order of first occurrence. *)
let rec free_vars = function
  | Num _ | Bool _ -> []
  | Id (x, t) -> [ (x, t) ]
  | BinOp (_, e1, e2, _) -> union (free_vars e1) (free_vars e2) (* includes application *)
  | UnOp (_, e, _) -> free_vars e
  | If (c, a, b, _) -> union (free_vars c) (union (free_vars a) (free_vars b))
  | Let (x, e1, e2, _) -> union (free_vars e1) (remove x (free_vars e2))
  | Abs (x, _, e, _) -> remove x (free_vars e)

(* ---------- Conversion ---------- *)

module C = Closure_ir

(* Hoisted functions, most recently created first (reversed at the end,
   so that the program lists them in creation order: inner funs first). *)
let hoisted : C.fundef list ref = ref []

let fun_count = ref 0
let new_fun_id () = let n = !fun_count in incr fun_count; n

(* [convert e]: [e] with every fun replaced by a MakeClosure; the funs
   themselves are added to [hoisted].
   Sub-expressions are converted with explicit lets, left to right:
   OCaml evaluates constructor arguments right to left, which would
   number the hoisted functions in reverse source order. *)
let rec convert e =
  match e with
  | Num n -> C.Num n
  | Bool b -> C.Bool b
  | Id (x, t) -> C.Var (x, t)
  | BinOp (Ast.App, f, arg, t) ->
      let f' = convert f in
      let arg' = convert arg in
      C.Apply (f', arg', t)
  | BinOp (op, e1, e2, t) ->
      let e1' = convert e1 in
      let e2' = convert e2 in
      C.BinOp (op, e1', e2', t)
  | UnOp (op, e1, t) -> C.UnOp (op, convert e1, t)
  | If (c, a, b, t) ->
      let c' = convert c in
      let a' = convert a in
      let b' = convert b in
      C.If (c', a', b', t)
  | Let (x, e1, e2, t) ->
      let e1' = convert e1 in
      let e2' = convert e2 in
      C.Let (x, e1', e2', t)
  | Abs (x, tx, body, t) ->
      let body' = convert body in            (* nested funs are hoisted first *)
      let env = free_vars e in               (* what this closure captures *)
      let id = new_fun_id () in
      hoisted := { C.id; param = (x, tx); env; body = body'; ret = type_of body } :: !hoisted;
      C.MakeClosure (id, env, t)

(* [convert_program e]: the whole program, closure converted. *)
let convert_program e =
  hoisted := [];
  fun_count := 0;
  let main = convert e in
  { C.funs = List.rev !hoisted; main }
