namespace LanguageConcepts

[<AutoOpen>]
module AExpr =
    open System 

    type AExpr =
        | CstI of int
        | Var of String
        | Add of AExpr * AExpr
        | Mul of AExpr * AExpr
        | Sub of AExpr * AExpr

    type Env = (string*int) list

    let rec lookup env key =
        match env with 
        | [] -> failwith "failed"
        | (k,v)::t -> if k=key then v else lookup t key
    
    let rec simplifyOpZero (e1:AExpr) = 
        match e1 with 
            | Add ( CstI 1  , CstI 0  )     -> Some( CstI 1  )
            | Add ( CstI 0  , CstI 1  )     -> Some( CstI 1  )
            | Add ( op1 , CstI 0  )         -> Some( op1 )
            | Add ( CstI 0  , op2 )         -> Some( op2 )
            | Sub ( op1 , CstI 0  )         -> Some( op1)
            | Sub ( Var v1 , Var v2 ) -> 
                Some( if v1=v2 then CstI 0 else Sub ( Var v1, Var v2 ) )
            | Mul ( CstI 1  , op2 )         -> Some( op2 )
            | Mul ( op1   , CstI 1  )       -> Some( op1 )
            | Mul ( CstI 0  , op2 )         -> Some( CstI 0 )
            | Mul ( op1 , CstI 0  )         -> Some( CstI 0 )
            | _ -> None
    
    let rec simplyRecurse (e1:AExpr) (e2:AExpr option) : AExpr = 
        let simplerOpZero = simplifyOpZero e1
        match simplerOpZero with 
            | Some(simpler)   -> (simplyRecurse simpler None ) 
            | None            -> e1                      
    
    let rec simply (e1:AExpr) : AExpr = 
        match e1 with 
            | Mul(l,r) ->
                let s1 = Mul((simplyRecurse l None) , (simplyRecurse r None)) 
                simplyRecurse s1 None
            | _        -> 
                simplyRecurse e1 None

    let simplify = simply 

    let rec fmt (e:AExpr) : string = 
        match e with 
        | CstI x                -> x.ToString()
        | Var x                 -> x
        | Add (e1,e2)           -> sprintf "(%s%s%s)" (fmt e1) "+" (fmt e2)
        | Mul (e1,e2)           -> sprintf "(%s%s%s)" (fmt e1) "*" (fmt e2)
        | Sub (e1,e2)           -> sprintf "(%s%s%s)" (fmt e1) "-" (fmt e2)
    
    // let fmts (e':AExpr) : string = simplify e'|> fmt 
    
    let rec Eval (e1:AExpr) (env:Env) : int = 
        let e = simplify e1
        match e with 
        | CstI x                  -> x
        | Var x                 -> (lookup env x)
        | Add (e1,e2)           -> (+) (Eval e1 env ) (Eval e2 env )
        | Mul (e1,e2)           -> (*) (Eval e1 env ) (Eval e2 env )
        | Sub (e1,e2)           -> (-) (Eval e1 env ) (Eval e2 env )

module AExprUnitTest =

    open System
    open Xunit

    [<Fact>]
    let ``evaluate x*(y+3) [x=1;y=2] should be 5 ``() = 
        // Exercises 1.2 (i)
        let ac = Eval (Mul( Var "x" , Add( Var "y", CstI 3) )) [("x",1) ; ("y",2)]
        Assert.Equal(5 , ac )
        ()

    [<Fact>]
    let ``fmt Var x should be "x" ``() = 
        // Exercises 1.2 (i)
        Assert.Equal("x" , fmt ( Var "x" ))
        Assert.Equal("(x-34)" , fmt ( Sub(Var "x" , CstI 34) ))
        ()
    
    [<Fact>]
    let ``evaluate v - ( w + z ) where [x=;y=...] should be 12 ``() = 
        // Exercises 1.2 (i)
        let ac = Eval (Mul( Var "x", (Add( Var "y", CstI 3) )) ) [("x",2) ; ("y",3)]
        Assert.Equal(2*(3+3) , ac )
        ()

    [<Fact>]
    let ``evaluate 2*(v-(w+z)) where [x=?...] should be ? 2*(2-(3+4)) ``() = 
        let ac = Eval  ( Mul (CstI 2 , ( Sub( Var "v", (Add( Var "w", Var "z") )) ))  ) [("v",2) ; ("w",3) ; ("z",4) ]
        Assert.Equal( 2*(2-(3+4)) , ac )
        ()

    [<Fact>]
    let ``evaluate x + y + z + v  where [x=?...] should be 2+3+5+7 ``() = 
        // x + y + z + v -> 
        let ac = Eval  (Add (Var "v" , (Add (Var "z" , (Add (Var "x" , Var "y")))))   )  [("x",2) ; ("y",3) ; ("z",5); ("v",7) ]
        Assert.Equal( 2+3+5+7 , ac )
        ()

    [<Fact>]
    let ``evaluate 2*3 should be 6 ``() = 
        // Exercises 1.2 (i)
        let ac = Eval (Mul( CstI 2, CstI 3) ) [("x",1) ; ("y",2)]
        Assert.Equal(6 , ac )
        ()

    let forCf(x:string) = x.Replace(" " , "")
    
    [<Theory>]
    [<InlineData(1,"1")>]
    [<InlineData(2,"2")>]
    let ``evaluate CI 1 should be "1" ``(i,s) = 
        Assert.Equal(s , fmt ( CstI  i  ))
        ()

    [<Theory>]
    [<InlineData(1,"1")>]
    [<InlineData(2,"2")>]
    let ``simplify (CI n ) should be "n" ``(i,s) = 
        let ac =  simplify (CstI i ) 
        Assert.Equal( s , fmt ( ac )  )
        ()

    [<Theory>]
    [<InlineData("e","e")>]
    [<InlineData("y","y")>]
    [<InlineData("E","E")>]
    [<InlineData("A","A")>]
    [<InlineData("ctr","ctr")>]
    let ``simplify (Var e ) should be "e" ``(v1,v2) = 
        Assert.Equal( v1 , fmt ( simplify( Var v2 ))  )
        ()

    [<Fact>]
    let ``simplify (1+0)(x+1) -> x``() = 
        let ac = simplify ( Mul ( (Add(CstI 1, CstI 0 )  ) , (Add(Var "x",CstI 0) ))  )
        Assert.Equal( "x"  , fmt ac)
        ()

    [<Fact>]
    let ``simply 0+1 -> 1``() = 
        Assert.Equal( CstI 1 , (simplify( Add( CstI 0 , CstI 1 )) ) )
        Assert.Equal( CstI 1 , (simplify( Add( CstI 1 , CstI 0 )) ) )
        Assert.Equal( Add( CstI 1 , CstI 1 ), (simply( Add( CstI 1 , CstI 1 )) ) )
        ()

    [<Fact>]
    let ``simplify 0+1 -> 1``() = 
        Assert.Equal( CstI 1 , (simplify( Add( CstI 1 , CstI 0 )) ) )
        Assert.Equal( CstI 1 , (simplify( Add( CstI 0 , CstI 1 )) ) )
        ()

    [<Fact>]
    let ``simplify e-e -> 0``() = 
        Assert.Equal( CstI 0 , (simplify( Sub( Var "x" , Var "x" )) ) )
        ()

    [<Fact>]
    let ``simply 0+e -> e``() = 
        Assert.Equal( Var "e" , (simplify( Add( CstI 0 , Var "e" )) ) )
        Assert.Equal( Var "e" , (simplify( Add( Var "e" , CstI 0 )) ) )
        ()

    [<Fact>]
    let ``e+0->e``() = 
        Assert.Equal( "x" , fmt ( simplify( Add( Var "x", CstI 0  ))  ))
        ()

    [<Fact>]
    let ``simply e-0 -> e``() = 
        Assert.Equal( Var "e" , (simplify( Sub( Var "e" , CstI 0 )) ) )
        ()

    [<Fact>]
    let ``simply e*1 -> e``() = 
        Assert.Equal( Var "e" , (simplify( Mul( Var "e" , CstI 1 )) ) )
        Assert.Equal( Var "e" , (simplify( Mul( CstI 1 , Var "e" )) ) )
        ()

    [<Fact>]
    let ``simplify e*o -> e``() = 
        Assert.Equal( CstI 0 , (simplify( Mul( Var "e" , CstI 0 )) ) )
        Assert.Equal( CstI 0 , (simplify( Mul( CstI 0 , Var "e" )) ) )
        ()

