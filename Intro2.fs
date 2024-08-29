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
        | _ -> failwith "unknown operator"
    | _ -> failwith "unknown data"

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
        | _ -> failwith "unknown operator"
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
let a = Sub(AVar "v", Add(AVar "w", AVar "z"))
//2 ∗ (v − (w + z))
let b =Mul(ACstI 2, Sub(AVar "v", Add(AVar "w", AVar "z")))
//x + y + z + v
let c = Add(AVar "v", Add(AVar "z", Add(AVar "y", AVar "x")))

let x1 = Mul(Add (AVar "x", ACstI 0), Add (ACstI 0, ACstI 1))

//1.2 iii
let rec fmt aexp =
    match aexp with
    | ACstI x -> x.ToString()
    | AVar x -> x
    | Add(e1, e2) -> "(" + (fmt e1) + " + " + (fmt e2) + ")"
    | Sub(e1, e2) -> "(" + (fmt e1) + " - " + (fmt e2) + ")"
    | Mul(e1, e2) -> "(" + (fmt e1) + " * " + (fmt e2) + ")"

//1.3 iv
let rec simplify aexp =
    let rec aux aexp =
        match aexp with
        | Add (ACstI 0, e) -> aux e
        | Add (e, ACstI 0) -> aux e
        | Sub (e, ACstI 0) -> aux e
        | Mul (ACstI 1,e) -> aux e
        | Mul (e, ACstI 1) -> aux e
        | Mul (e, ACstI 0) -> ACstI 0
        | Mul (ACstI 0, e) -> ACstI 0
        | Sub (e1, e2) when e1 = e2 -> ACstI 0
        | Mul (e1, e2) -> Mul ((aux e1), (aux e2)) 
        | _ -> aexp
    let result = aux aexp
    if aexp = result then result
    else simplify result 
    

    
//1.4 
import java.util.Map;
import java.util.HashMap;

public class Main {
    public static void main(String[] args) {
        Map<String, Integer> env = Map.of("z", 3, "x", 4, "y", 7, "a", 2, "b", 3);


        Expr e = new Add(new CstI(17), new Var("z"));
        System.out.println(e.toString());  
        System.out.println(e.eval(env));

        // Expression 1: 5 * x
        Expr e1 = new Mul(new CstI(5), new Var("x"));
        System.out.println(e1.toString()); 
        System.out.println(e1.eval(env)); 

        // Expression 2: (10 + y) - 3
        Expr e2 = new Sub(new Add(new CstI(10), new Var("y")), new CstI(3));
        System.out.println(e2.toString()); 
        System.out.println(e2.eval(env));  

        // Expression 3: (a * b) + 1
        Expr e3 = new Add(new Mul(new Var("a"), new Var("b")), new CstI(1));
        System.out.println(e3.toString());
        System.out.println(e3.eval(env)); 
    }
}

abstract class Expr {
    public abstract String toString(); 
    public abstract int eval(Map<String, Integer> env);

}

class CstI extends Expr {
    protected final int i;

    public CstI(int i) {
        this.i = i;
    }

    @Override
    public int eval(Map<String, Integer> env) {
        return i; 
    }

    @Override
    public String toString() { 
        return "CstI " + Integer.toString(i); 
    } 
}

class Var extends Expr {
    protected final String name;

    public Var(String name) {
        this.name = name;
    }

    public int eval(Map<String, Integer> env) {
        if (env.containsKey(name)) {
            return env.get(name);
        } else {
            throw new RuntimeException("Variable not found: " + name);
        }
    }

    @Override
    public String toString() { 
        return "Var " + name;
    } 
}

abstract class Binop extends Expr { 
    protected final Expr e1, e2; 

    public Binop(Expr e1, Expr e2) { 
        this.e1 = e1;
        this.e2 = e2;
    }
}

class Add extends Binop {
    public Add(Expr e1, Expr e2) {
        super(e1, e2);
    }

    public int eval(Map<String, Integer> env) {
        return e1.eval(env) + e2.eval(env);
    }

    @Override
    public String toString() { 
        return "Add(" + e1.toString() + ", " + e2.toString() + ")";
    } 
}

class Mul extends Binop {
    public Mul(Expr e1, Expr e2) {
        super(e1, e2);
    }
    
    public int eval(Map<String, Integer> env) {
        return e1.eval(env) * e2.eval(env);
    }
    
    @Override
    public String toString() { 
        return "Mul(" + e1.toString() + ", " + e2.toString() + ")";
    } 
}

class Sub extends Binop {
    public Sub(Expr e1, Expr e2) {
        super(e1, e2);
    }

    public int eval(Map<String, Integer> env) {
        return e1.eval(env) - e2.eval(env);
    }
    
    @Override
    public String toString() { 
        return "Sub(" + e1.toString() + ", " + e2.toString() + ")";
    } 
}





