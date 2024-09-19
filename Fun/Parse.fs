(* Lexing and parsing of micro-ML programs using fslex and fsyacc *)

module Parse

open System
open System.IO
open System.Text
open FSharp.Text.Lexing
open Absyn

(* Plain parsing from a string, with poor error reporting *)

let fromString (str: string) : expr =
    let lexbuf = (*Lexing. insert if using old PowerPack *)
        LexBuffer<char>.FromString(str)

    try
        FunPar.Main FunLex.Token lexbuf
    with exn ->
        let pos = lexbuf.EndPos
        failwithf "%s near line %d, column %d\n" (exn.Message) (pos.Line + 1) pos.Column

(* Parsing from a file *)

let fromFile (filename: string) =
    use reader = new StreamReader(filename)

    let lexbuf = (* Lexing. insert if using old PowerPack *)
        LexBuffer<char>.FromTextReader reader

    try
        FunPar.Main FunLex.Token lexbuf
    with exn ->
        let pos = lexbuf.EndPos
        failwithf "%s in file %s near line %d, column %d\n" (exn.Message) filename (pos.Line + 1) pos.Column

(* Exercise it *)

let e1 = fromString "5+7"
let e2 = fromString "let f x = x + 7 in f 2 end"

(* Examples in concrete syntax *)

let ex1 = fromString @"let f1 x = x + 1 in f1 12 end"

(* Example: factorial *)

let ex2 =
    fromString
        @"let fac x = if x=0 then 1 else x * fac(x - 1)
              in fac n end"

(* Example: deep recursion to check for constant-space tail recursion *)

let ex3 =
    fromString
        @"let deep x = if x=0 then 1 else deep(x-1) 
              in deep count end"

(* Example: static scope (result 14) or dynamic scope (result 25) *)

let ex4 =
    fromString
        @"let y = 11
              in let f x = x + y
                 in let y = 22 in f 3 end 
                 end
              end"

(* Example: two function definitions: a comparison and Fibonacci *)

let ex5 =
    fromString
        @"let ge2 x = 1 < x
              in let fib n = if ge2(n) then fib(n-1) + fib(n-2) else 1
                 in fib 25 
                 end
              end"

//4.2
let sum = fromString "let sum x = if x=1 then x else sum(x+1) in sum count end"
// eval (fromString "let sum x = if x=1 then x else x+ sum(x-1) in sum count end") [("count", Int 1000)];;
// val it: int = 500500

let eight3 = fromString "let pow x = if x=0 then 1 else 3*pow(x-1) in pow exponent end"
//eval (fromString "let pow x = if x=0 then 1 else 3*pow(x-1) in pow exponent end") [("exponent", Int 8)];;

let powsum3 = fromString 
        @"let pow3 x = if x = 0 then 1 else 3 * pow3 (x - 1)
            in let powsum x = if x=0 then 1 else pow3 x + powsum (x - 1)
                in powsum 11 
                end
            end"
// eval (fromString "let pow3 x = if x = 0 then 1 else 3 * pow3 (x - 1) in let powsum x = if x=0 then 1 else pow3 x + powsum (x - 1) in powsum 11 end end") []
// val it: int = 265720

let pow8 = fromString 
        @"let powx x y = if y = 0 then 1 else x * powx x (y - 1)
            in let powsum x = if x = 10 then powx 10 8 else powx x 8 + powsum (x + 1)
                in powsum 1 
                end
            end"

// eval (fromString "let powx x y = if y = 0 then 1 else x * powx x (y - 1) in let powsum z = if z = 10 then powx 10 8 else powx z 8 + powsum (z + 1) in powsum 1 end end") [];;