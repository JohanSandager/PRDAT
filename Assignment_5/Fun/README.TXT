Compiling and loading the micro-ML evaluator and parser (Fun/README.TXT)
------------------------------------------------------------------------

The items A to C are similar to the code found under folder Fun handed
out last week. In this week we extend with higher order functions,
items D-E and type interence, item F.

A. Loading the micro-ML evaluator, with abstract syntax only

   dotnet fsi Absyn.fs Fun.fs

   open Absyn;;
   open Fun;;
   let res = run (Prim("+", CstI 5, CstI 7));;
   #q;;


B. Generating and compiling the lexer and parser, and loading them:

   fsyacc --module FunPar FunPar.fsy
   fslex --unicode FunLex.fsl
   dotnet fsi -r ~/fsharp/FsLexYacc.Runtime.dll Util.fs Absyn.fs FunPar.fs FunLex.fs Parse.fs   

   open Parse;;
   let e1 = fromString "5+7";;
   let e2 = fromString "let y = 7 in y + 2 end";;
   let e3 = fromString "let f x = x + 7 in f 2 end";;


C. Using the lexer, parser and first-order evaluator together:

   fsyacc --module FunPar FunPar.fsy
   fslex --unicode FunLex.fsl
   dotnet fsi -r ~/fsharp/FsLexYacc.Runtime.dll Util.fs Absyn.fs FunPar.fs FunLex.fs Parse.fs Fun.fs ParseAndRun.fs

   open ParseAndRun;;
   run (fromString "5+7");;
   run (fromString "let y = 7 in y + 2 end");;
   run (fromString "let f x = x + 7 in f 2 end");;


D. Loading the evaluator for a higher-order functional language (same
   abstract syntax as the first-order language):

   dotnet fsi Absyn.fs HigherFun.fs

   open HigherFun;;
   eval ex1 [];;
   open Absyn;;
   run (Letfun ("twice", "f",
                Letfun ("g", "x", Call (Var "f", Call (Var "f", Var "x")), Var "g"),
                        Letfun ("mul3", "z", Prim ("*", Var "z", CstI 3),
                                Call (Call (Var "twice",Var "mul3"),CstI 2))));;

   (The above abstract syntax term corresponds to the concrete syntax
   term shown in point E below).


E. Using the lexer, parser and higher-order evaluator together:

   fsyacc --module FunPar FunPar.fsy
   fslex --unicode FunLex.fsl	
   dotnet fsi -r ~/fsharp/FsLexYacc.Runtime.dll Util.fs Absyn.fs FunPar.fs FunLex.fs Parse.fs HigherFun.fs ParseAndRunHigher.fs

   open ParseAndRunHigher;;
   run (fromString @"let twice f = let g x = f(f(x)) in g end 
                     in let mul3 z = z*3 in twice mul3 2 end end");;

   //6.1
   run (fromString @"let add x = let f y = x+y in f end
                     in add 2 5 end");;

   run (fromString @"let add x = let f y = x+y in f end
                     in let addtwo = add 2
                        in addtwo 5 end
                     end");;

   run (fromString @"let add x = let f y = x+y in f end
                     in let addtwo = add 2
                        in let x = 77 in addtwo 5 end
                     end end");;

   run (fromString @"let add x = let f y = x+y in f end
                     in add 2 end");;
   

F. Using the lexer, parser and polymorphic type inference together:

   fsyacc --module FunPar FunPar.fsy
   fslex --unicode FunLex.fsl
   dotnet fsi -r ~/fsharp/FsLexYacc.Runtime.dll Util.fs Absyn.fs FunPar.fs FunLex.fs Parse.fs TypeInference.fs ParseAndType.fs

   open ParseAndType;;
   inferType (fromString "let f x = 1 in f 7 + f false end");; 
   Answer: type int

   //6.3
   //6.3_1
   inferType ( fromString "let f x = 1 in f f end");;
   Answer: type int
   
   inferType ( fromString "let f g = g g in f end");;
   Answer: g is both the argument and the function. It creates a infinite loop when calling g g.

   inferType ( fromString "let f x = let g y = y in g false end in f 42 end");;
   Answer: type bool

   inferType (fromString "let fx = let g y = if true then y else x in g false end in f 42 end");;
   Answer: y is boolean and x is int, when doing if-else the type must be the same 

   inferType (fromString "let f x = let g y = if true then y else x in g false end in f true end");;
   Answer: here both y and x is bools and therefor it works. type bool

   //6.3_2 
   bool -> bool
   inferType (fromString "let f x = if x then true else false in f true end" );;

   int -> int 
   inferType (fromString "let f x = if (x < 42) then 42 else 0 in f 100 end" );;

   int -> int -> int
   inferType (fromString "let f x = let g y = x+y in g 42 end in f 42 end");;
   let f x =
      let g y = y + x
      in g 42 end
   in f 42 end

   //i dunno anymore if they are good
   a' -> b' -> a'
   inferType (fromString "let f x = let g y = x in g 42 end in f true end" );;
   inferType (fromString "let f x = let g y = x in g true end in f 42 end" );;
   let f x =
      let g y = x
      in g 42 end
   in f true end

   ’a -> ’b -> ’b
   inferType (fromString "let f x = let g y = y in g true end in f 42 end" );;
   inferType (fromString "let f x = let g y = y in g 42 end in f true end" );;
   let f x =
      let g y = y
      in g 42 end
   in f true end;;

   (’a -> ’b) -> (’b -> ’c) -> (’a -> ’c)


   ’a -> ’b

   ’a