[<AutoOpen>]
module LangConcept01_3 = 

    type Expr = 
        | CI of int
        | Prim of string * Expr * Expr

    let rec Eval (e:Expr) : int = 
        match e with 
        | CI x -> x
        | Prim ("+",e1,e2) -> (+) (Eval e1) (Eval e2)
        | Prim ("-",e1,e2) -> (-) (Eval e1) (Eval e2)
        | Prim ("*",e1,e2) -> (*) (Eval e1) (Eval e2)
        | Prim _ -> failwith "primitive not recognised"

let e1  = CI 1
let e2  = Prim ("+" , CI 1, CI 2)
let e3  = Prim ("-" , CI 1, CI 2)
let e4  = Prim ("*" , CI 1, CI 2)

Eval e1
Eval e2
Eval e3
Eval e4






