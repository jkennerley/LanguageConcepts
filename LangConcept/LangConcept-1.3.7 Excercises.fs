[<AutoOpen>]
module LanguageConcepts 
    open System
    
    type AExpr =
        | CI of int
        | Var of String
        | Add of AExpr * AExpr
        | Mul of AExpr * AExpr
        | Sub of AExpr * AExpr

    type Env = (string*int) list

    let rec lookup env key =
        match env with 
        | [] -> failwith "failed"
        | (k,v)::t -> if k=key then v else lookup t key

    let simplify(e:AExpr) : AExpr = 
        match e with 
        | Add ( Var x, CI 0 )      -> Var x
        | Add ( e1   , CI 0 )      -> e1
        | _-> e

    let rec Eval (e':AExpr) (env:Env) : int = 
        let e = simplify e'
        match e with 
        | CI x                  -> x
        | Var x                 -> (lookup env x)
        | Add (e1,e2)           -> (+) (Eval e1 env ) (Eval e2 env )
        | Mul (e1,e2)           -> (*) (Eval e1 env ) (Eval e2 env )
        | Sub (e1,e2)           -> (-) (Eval e1 env ) (Eval e2 env )

    let rec fmt (e:AExpr) : string = 
        match e with 
        | CI x                  -> x.ToString()
        | Var x                 -> x
        | Add (e1,e2)           -> sprintf "(%s %s %s)" (fmt e1) "+" (fmt e2)
        | Mul (e1,e2)           -> sprintf "(%s %s %s)" (fmt e1) "*" (fmt e2)
        | Sub (e1,e2)           -> sprintf "(%s %s %s)" (fmt e1) "-" (fmt e2)

    let fmts (e':AExpr) : string = 
        simplify e'
        |> fmt 

module LanguageConceptsUnitTest 
//    open Xunit
//    open System
    let asb()=
        ()



//// Exercises 1.2 (i)
//let ex121a = Eval (Mul( CI 2, CI 3) ) [("x",1) ; ("y",2)]
//Assert.Equal(2*3 , ex121a )
//
//// y+3
//let ex121b = Eval (Add( Var "y", CI 3) ) [("x",2) ; ("y",3)]
//Assert.Equal(2*3 , ex121b)
//
//// x * ( y + 3 )
//let ex121c = Eval (Mul( Var "x", (Add( Var "y", CI 3) )) ) [("x",2) ; ("y",3)]
//Assert.Equal(2*(3+3) , ex121c)
//
//// Exercises 1.2 (ii)
// v - ( w + z ) -> 2 - (3 + 4)
//let ex123a = Eval (Sub( Var "v", (Add( Var "w", Var "z") )) ) [("v",2) ; ("w",3) ; ("z",4) ]
//Assert.Equal(2 - (3 + 4), ex123a )
//
//// 2*( v - ( w + z )) -> 2* (2 - (3 + 4))
//Eval  ( Mul (CI 2 , ( Sub( Var "v", (Add( Var "w", Var "z") )) ))  ) [("v",2) ; ("w",3) ; ("z",4) ]
//
//// x + y + z + v -> 
//Eval  (Add (Var "v" , (Add (Var "z" , (Add (Var "x" , Var "y")))))   )  [("x",2) ; ("y",3) ; ("z",5); ("v",7) ]
//
////
//fmt ( CI   1  )
//Assert.Equal("1" , fmt ( CI   1  ))
//
////
// fmt ( Var "x" )
//Assert.Equal("x" , fmt ( Var "x" ))
//
//let forCf(x:string) = x.Replace(" " , "")
//forCf "(x + 1)"
//forCf "(X + 1)"
//
////
//fmt ( Add (Var "x" , CI 1) )
//Assert.Equal( "(x+1)" |> forCf , fmt ( Add (Var "x" , CI 1) )  |> forCf )
//
/////////////////////////////////////
////simplify( CI   1  )
//Assert.Equal( "1" |> forCf , fmt ( simplify( CI   1  ))  |> forCf )
//
////
//simplify( Var "x" )
//Assert.Equal( "x" |> forCf , fmt ( simplify( Var "x" ) )  |> forCf )
//
////
//simplify( Add (Var "x" , CI 0) )
//Assert.Equal( "x" |> forCf , fmt ( simplify( Add (Var "x" , CI 0) ) )  |> forCf )
//
////
//simplify( Add (Var "x" , CI 1) )
//Assert.Equal( "(x+1)" |> forCf , fmt ( simplify( Add (Var "x" , CI 1) ) )  |> forCf )
//
////
// simplify( Add (CI 1 , CI 1) )
// 
//fmt ( simplify( Add (CI 1 , CI 1) ) )
//Assert.Equal( "(1+1)" |> forCf , fmt ( simplify( Add (CI 1 , CI 1) ) )|> forCf )
//
////
//simplify( Add (CI 1 , CI 0) )
//fmt ( simplify( Add (CI 1 , CI 0) ) )
//Assert.Equal( "1" |> forCf , fmt ( simplify( Add (CI 1 , CI 0) ) )|> forCf )
//
////
//fmt ( simplify( Add (Var "x" , CI 1) ) )
//Assert.Equal( "(x+1)" |> forCf ,  fmt ( simplify( Add (Var "x" , CI 1) ) )|> forCf )
//
////
//fmt ( simplify( Add (Var "x" , CI 0) ) )
//Assert.Equal( "x" |> forCf ,  fmt ( simplify( Add (Var "x" , CI 0) ) )|> forCf )
//
//// iv
// (1+0).(x+0) -> x
//Add( CI 1 , CI 0 )
//Add( Var "X"  , CI 0 )
//
//fmt ( simplify ( Add( CI 1 , CI 0 )) )
//fmt ( simplify ( Add( Var "x"  , CI 0 ) ) )
//
//fmt ( simplify( Mul ( Add( CI 1 , CI 0 ) , ( Add( Var "x"  , CI 0 ) ) ) )     )
//
////
//fmts ( Add (Var "x" , CI 0) )
//Assert.Equal( "x" |> forCf ,  fmts ( Add (Var "x" , CI 0) )|> forCf )
//
//
//// (1+0)*(x+0)
//fmts ( Mul (CI 1 , Var "x" ) )
//fmts ( Mul ( (Add(CI 1, CI 0 )  ) , (Add(Var "x",CI 0) )) )
//
//(*
//0+e
//e+0
//e-0
//1*e
//e*1
//0*e
//e*0
//e-e
//*)

