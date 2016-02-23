namespace LanguageConcepts

[<AutoOpen>]
module TokenizerParser =
    open System 

    type Token =
        | ID of string
        | INT of int 
        | HAT 
        | PLUS 
        | MINUS 

    let regex s =  System.Text.RegularExpressions.Regex(s)

    let tokenR = regex @"((?<token>(\d+|\w+|\^|\+|-))\s*)*"

    let tokenize (s : string) = 
        [for x in tokenR.Match(s).Groups.["token"].Captures do 
             let token = 
                 match x.Value with 
                 | "^" -> HAT 
                 | "-" -> MINUS
                 | "+" -> PLUS
                 | s when System.Char.IsDigit s.[0] -> INT (int s)
                 | s -> ID s 
             yield token]

    type Term =
        | Term of int * string * int
        | Const of int

    type Polynomial = Term list
    type TokenStream = Token list

    let tryToken (src : TokenStream) =
        match src with
        | tok :: rest -> Some(tok, rest)
        | _ -> None

    let parseIndex src =
        match tryToken src with
        | Some (HAT, src) ->
            match tryToken src with
            | Some (INT num2, src) ->
                num2, src
            | _ -> failwith "expected an integer after '^'"
        | _ -> 1, src

    let parseTerm src =
        match tryToken src with
        | Some (INT num, src) ->
            match tryToken src with
            | Some (ID id, src) ->
               let idx, src = parseIndex src
               Term (num, id, idx), src
            | _ -> Const num, src
        | Some (ID id, src) ->
             let idx, src = parseIndex src
             Term(1, id, idx), src
        | _ -> failwith "end of token stream in term"

    let rec parsePolynomial src =
        let t1, src = parseTerm src
        match tryToken src with
        | Some (PLUS, src) ->
            let p2, src = parsePolynomial src
            (t1 :: p2), src
        | _ -> [t1], src

    let parse input =
        let src = tokenize input
        let result, src = parsePolynomial src
        match tryToken src with
        | Some _ -> failwith "unexpected input at end of token stream!"
        | None -> result

module TokenizerTest =
    open System
    open Xunit

    [<Fact>]
    let ``tokenize expresson should yield expected``() = 
        let tokens = tokenize "x^5 - 2x^3 + 20" 

        let t1 = tokens.Item(0)

        Assert.Equal( tokens.Item(0) , ID "x" )
        Assert.Equal( tokens.Item(1) , HAT  )
        Assert.Equal( tokens.Item(2) , INT 5 )
        Assert.Equal( tokens.Item(3) , MINUS )
        Assert.Equal( tokens.Item(4) , INT 2 )
        Assert.Equal( tokens.Item(5) , ID "x" )
        Assert.Equal( tokens.Item(1) , HAT  )
        Assert.Equal( tokens.Item(7) , INT 3 )
        Assert.Equal( tokens.Item(8) , PLUS )
        Assert.Equal( tokens.Item(9) , INT 20 )
        ()

    [<Fact>]
    let ``parse should yield expected``() = 
        let ast = parse "1+3"
        Assert.Equal( ast.Item(0) , Const 1 )
        Assert.Equal( ast.Item(1) , Const 3 )
        ()

