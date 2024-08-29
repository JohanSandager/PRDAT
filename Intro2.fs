(* Programming language concepts for software developers, 2010-08-28 *)

(* Evaluating simple expressions with variables *)

module Intro2

(* Association lists map object language variables to their values *)

let max val1 val2 = if val1 > val2 then val1 else val2
let min val1 val2 = if val1 < val2 then val1 else val2
let eq val1 val2 = if val1 = val2 then 1 else 0


let env = [ ("a", 3); ("c", 78); ("baf", 666); ("b", 111) ]

let emptyenv = [] (* the empty environment *)

let rec lookup env x =
    match env with
    | [] -> failwith (x + " not found")
    | (y, v) :: r -> if x = y then v else lookup r x

let cvalue = lookup env "c"


(* Object language expressions with variables *)

type expr =
    | CstI of int
    | Var of string
    | Prim of string * expr * expr
    | If of expr * expr * expr

let e1 = CstI 17

let e2 = Prim("+", CstI 3, Var "a")

let e3 = Prim("+", Prim("*", Var "b", CstI 9), Var "a")


(* Evaluation within an environment *)

let rec eval e (env: (string * int) list) : int =
    match e with
    | CstI i -> i
    | Var x -> lookup env x
    | Prim("+", e1, e2) -> eval e1 env + eval e2 env
    | Prim("*", e1, e2) -> eval e1 env * eval e2 env
    | Prim("-", e1, e2) -> eval e1 env - eval e2 env
    | Prim("max", e1, e2) -> max (eval e1 env) (eval e2 env)
    | Prim("min", e1, e2) -> min (eval e1 env) (eval e2 env)
    | Prim("==", e1, e2) -> eq (eval e1 env) (eval e2 env)
    | Prim _ -> failwith "unknown primitive"

let e1v = eval e1 env
let e2v1 = eval e2 env
let e2v2 = eval e2 [ ("a", 314) ]
let e3v = eval e3 env


// 1.1 ii
eval (Prim("max", CstI 1, CstI 2)) []
eval (Prim("min", CstI 2, CstI 1)) []
eval (Prim("==", CstI 1, CstI 1)) []

// 1.1 iii
let rec eval2 e (env: (string * int) list) : int =
    match e with
    | CstI i -> i
    | Var x -> lookup env x
    | Prim(op, e1, e2) ->
        let i1 = eval2 e1 env
        let i2 = eval2 e2 env

        match op with
        | "+" -> i1 + i2
        | "-" -> i1 - i2
        | "*" -> i1 * i2
        | "max" -> max i1 i2
        | "min" -> min i1 i2
        | "==" -> eq i1 i2
    | Prim _ -> failwith "unknown primitive"

// 1.1 iv && v
let rec eval3 e (env: (string * int) list) : int =
    match e with
    | CstI i -> i
    | Var x -> lookup env x
    | Prim(op, e1, e2) ->
        let i1 = eval2 e1 env
        let i2 = eval2 e2 env

        match op with
        | "+" -> i1 + i2
        | "-" -> i1 - i2
        | "*" -> i1 * i2
        | "max" -> max i1 i2
        | "min" -> min i1 i2
        | "==" -> eq i1 i2
    | Prim _ -> failwith "unknown primitive"
    | If(e1, e2, e3) -> if eval e1 env = 0 then eval e3 env else eval e2 env


//1.2 (i)
type aexpr =
    | ACstI of int
    | AVar of string
    | Add of aexpr * aexpr
    | Mul of aexpr * aexpr
    | Sub of aexpr * aexpr


//1.2 ii
//v − (w + z)
Sub(AVar "v", Add(AVar "w", AVar "z"))
//2 ∗ (v − (w + z))
Mul(ACstI 2, Sub(AVar "v", Add(AVar "w", AVar "z")))
//x + y + z + v
Add(AVar "v", Add(AVar "z", Add(AVar "y", AVar "x")))

let rec fmt aexp =
    match aexp with
    | ACstI x -> x.ToString()
    | AVar x -> x
    | Add(e1, e2) -> " ( " + (fmt e1) + " + " + (fmt e2) + ")"
    | Sub(e1, e2) -> " ( " + (fmt e1) + " - " + (fmt e2) + " ) "
    | Mul(e1, e2) -> " ( " + (fmt e1) + " * " + (fmt e2) + " ) "

let rec simplify aexp =
    match aexp with
    | Add (0, e) -> e
    | Add (e, 0) -> e
    | Sub (e, 0) -> e
    | Mul (1,e) -> e
    | Mul (e,1) -> e
    | Mul (e, 0) -> 0
    | Mul (0, e) -> 0
    | Sub (e1, e2) when e1 = e2 -> 0


