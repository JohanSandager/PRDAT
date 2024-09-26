Compiling and loading the typed micro-ML evaluator (TypedFun/README.TXT)
------------------------------------------------------------------------

A. Loading evaluator and type checker for the explicitly typed language

   dotnet fsi TypedFun.fs

   open TypedFun;;

   // These typechecks should succeed:
   typeCheck (Prim("+", CstI 5, CstI 7));;
   typeCheck ex1;;
   typeCheck 

   // This typecheck should throw exception:
   typeCheck exErr1;;

   //excercissse 5.7 
   typeCheck (ListExpr ([CstB true; CstB false], TypB));;
   typeCheck (ListExpr ([CstI 10 true; CstI 10], TypB));;
   typeCheck (ListExpr ([CstI 10 true; CstI 10], TypI));;

   #q;;
