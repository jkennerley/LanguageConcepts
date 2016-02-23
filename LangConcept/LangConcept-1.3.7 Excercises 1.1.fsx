open System

[<AutoOpen>]
module LangConcept137 = 
    type Expr =
        | CI of int
        | Var of String
        | Conditional of Expr * Expr * Expr
        | Prim of string * Expr * Expr

    type Env = (string*int) list

    let rec lookup env key =
        match env with 
        | [] -> failwith "failed"
        | (k,v)::t -> if k=key then v else lookup t key

    let rec Eval (e:Expr) (env:Env) : int = 
        match e with 
        | CI x                  -> x
        | Var x                 -> (lookup env x)
        | Prim (ope,e1,e2)     -> 
            let i1 = (Eval e1 env )
            let i2 = (Eval e2 env )
            match ope with 
                | "+"   -> (+) i1  i2
                | "-"   -> (-) i1  i2
                | "*"   -> (*) i1 i2
                | "max" -> Math.Max (i1, i2)
                | "min" -> Math.Min (i1, i2)
                | "=="  -> if i1 = i2  then 1 else 0
        | Conditional (e1,e2,e3)     -> 
            let i1 = (Eval e1 env )
            if i1=1 then (Eval e2 env ) else ((Eval e3 env ))
        | Prim _ -> failwith "primitive not recognized"

// environment 
let env = [("a",3);("b",78) ; ("c",78)]

// 17
let e1 = CI 1
// x
let e2 = Var "a"
// 3+a
let e3 = Prim ("+" , CI 3 , Var "a")
// b.9+a
78*9 + 3


let e4 = Prim ("+" , Prim ("*" , Var "b",CI 9) , Var "a")


// evaluate
Eval e1 env
Eval e2 env
Eval e3 env
Eval e4 env

// Exercises 1.7 (ii)
Eval (Prim ("+" , CI 1 , CI 1)) []
Eval (Prim ("max" , CI 1 , CI 2)) []
Eval (Prim ("max" , CI 3 , CI 2)) []

Eval (Prim ("min" , CI 1 , CI 2)) []
Eval (Prim ("min" , CI 3 , CI 2)) []

Eval (Prim ("==" , CI 1 , CI 1)) []
Eval (Prim ("==" , CI 1 , CI 2)) []

// Exercises 1.7 (iii)

// Exercises 1.7 (iv) :: triadic Conditional
Eval (Conditional( CI 0 , CI 1 , CI 2)) []
Eval (Conditional( CI 1 , CI 1 , CI 2)) []

// Exercises 1.7 (v) :: triadic Conditional

Eval (Conditional( CI 0 , CI +1 , CI -1)) []
Eval (Conditional( CI 1 , CI +1 , CI -1)) []
// if a=1 => +1
Eval (Conditional( Var "a" , CI +1 , CI -1)) [("a",1)]
// if a=0 => -1
Eval (Conditional( Var "a" , CI +1 , CI -1)) [("a",0)]

// excercises 1.2 





