open System

[<AutoOpen>]
module LangConcept135 = 

(*

Can represent expression using objects using OOP inheritence.

Expr{ i:int ; Eval: virtual Expr->int}
Var{ Name:string ; Eval: virtual Expr->int } : Expr
Prim{ Op:string  ; e1:Expr ; e2:Expr ; Eval:Expr->int}


use overidable Eval method and abstract base class

Using this OOP method rather than DU and pattern matching will increase complexity.

*)
