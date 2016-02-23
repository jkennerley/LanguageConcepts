open System

[<AutoOpen>]
module LangConcept133 = 

(*

Syntax ::
  Is program text well formed

Semantics 
  What happens on execution

Concrete Syntax
  e.g. "3+a"
  includes whitespace, brackets, curlys, 

Abstract Syntax
  the tree version of concrete syntax
  the tree could DU tree or object structure
  e.g. the representation of "3+1" as Prim ("+" , CI 3 , Var "a")
  The AST simplifies processing, interpretation, compilation
  Problem : concreteSyntax -> abstractSyntax

Dynamic Semantics (Execution)
  runtime effect e.g after evaluating AST
   
Static Semantics (Checks)
  compile time correctness 
  those properties that can be checked without execution
  are all variables declared ?
  are operators used with operands of correct type
  type inference ...
  e.g. x1 is a legal variable name but is it declared twice, 


Some small functional languages ..

First order functional language
Higher order functional language
Subset of c 
A subset of Icon

Review concrete syntax with lexer and parser.
fslex and fsyacc

The dynamic semantics can deduced be either by eval or compilation
Compilation is translation to another language e.g stack machine code.
Compilation is usually by functions called comp.
 
JVM and .Net CLI are 2 example abstract stack machines.

*)