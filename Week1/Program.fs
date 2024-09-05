// For more information see https://aka.ms/fsharp-console-apps

open Intcomp1

printfn "Hello from F#"

//let e1 = Let1([ ("z", CstI1 17); ("x", CstI1 10) ], Prim1("+", Var1 "z", Var1 "x"))


let e2 = Let("z", CstI 17, Prim("+", Var "z", Var "z"))
let e3 = Let1([ "z", CstI1 17 ], Prim1("+", Var1 "z", Var1 "z"))

let e4 =
    Let1([ ("x1", Prim1("+", Var1 "x1", CstI1 7)) ], Prim1("+", Var1 "x1", CstI1 8))

(*
printfn "%d" (List.length (freevars e2))
printfn "%d" (List.length (freevars1 e3))

printfn "%d" (List.length (freevars1 e4))
tcomp e1 []*)

let e17 =
    Let1(
        [ ("x1", Prim1("+", CstI1 5, CstI1 7)); ("x2", Prim1("*", Var1 "x1", CstI1 2)) ],
        Prim1("+", Var1 "x1", Var1 "x2")
    )

printfn "Test e17. Expected result is empty list. : %A" (tcomp1 e3 [])
