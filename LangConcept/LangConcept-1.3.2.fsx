open System

[<AutoOpen>]
module LangConcept132 = 

    type Env = (string*int) list

    let rec lookup env key =
        match env with 
        | [] -> failwith "failed"
        | (k,v)::t -> if k=key then v else lookup t key

    type Expr =
        | CI of int
        | Var of String
        | Prim of string * Expr * Expr

    let rec Eval (e:Expr) (env:Env) : int = 
        match e with 
        | CI x -> x
        | Var x -> (lookup env x)
        | Prim ("+",e1,e2) -> (+) (Eval e1 env ) (Eval e2 env )
        | Prim ("-",e1,e2) -> (-) (Eval e1 env ) (Eval e2 env )
        | Prim ("*",e1,e2) -> (*) (Eval e1 env ) (Eval e2 env )
        | Prim _ -> failwith "primitive not recognized"

    //let get env key =
    //    match (env |> List.tryFind (fun x -> fst x = key)) with 
    //        | Some (_,x) -> Some(x)
    //        | _-> None

// 17
let e1 = CI 1
// x
let e2 = Var "a"
// 3+a
let e3 = Prim ("+" , CI 3 , Var "a")
// b.9+a
let e4 = Prim ("+" , Prim ("*" , Var "b",CI 9) , Var "a")

// environment 
let env = [("a",3);("b",78)]

// evaluate
Eval e1 env
Eval e2 env
Eval e3 env
Eval e4 env

