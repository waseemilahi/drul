(* Drul_printer package
    Pretty-print a Drul AST
    11/11/2008
*)

open Drul_ast

let string_of_intop = function
        Add -> "Addition"
    |   Sub -> "Subtraction"
    |   Mult ->"Multiplication"
    |   Div  -> "Division"
    |   Mod  -> "Modulus"
    
let string_of_compop = function
        EqualTo -> "Equality test"
    |   NotEqual -> "Inequality test"
    |   LessThan -> "Less than"
    |   GreaterThan ->"Greater than"
    |   LessEq  -> "Less than/equal to"
    |   GreaterEq -> "Greater than/equal to"

let string_of_boolop = function 
        And -> "Conjunction"
    |   Or -> "Disjunction"


let rec string_of_expr = function
        CInt(x) -> "Constant integer " ^ string_of_int(x)
    |   CStr(s) -> "Constant string [" ^  s ^"]"
    |   CBool(b)-> "Constant " ^ if b then "TRUE" else "FALSE"
    |   Var(id) -> "Variable name" ^ id
    |   UnaryMinus(neg) -> "Arithmetic negation of " ^ string_of_expr(neg)
    |   UnaryNot(bool)  -> "Logical negation of " ^ string_of_expr(bool)
    |   ArithBinop(a,op,b) ->"Arithmetic operation: " ^ string_of_intop(op)
                ^ ": left operand= "^ string_of_expr(a) 
                ^ "; right operand= " ^ string_of_expr(b)
    |   LogicBinop(a,op,b) -> string_of_boolop(op) ^ " of " ^ string_of_expr(a) 
                ^ " with "^ string_of_expr(b)
    |   Comparison(a,op,b) -> "Comparison of type " ^ string_of_compop(op) 
                ^ ": left operand= " ^ string_of_expr(a)
                ^ "; right operand= " ^ string_of_expr(b)
    |   FunCall(name,arglist) -> "Call to function '"^name 
        ^ "' with these arguments: " 
        ^ List.fold_left (fun a ex -> a ^ string_of_expr(ex) ^ "; ") "" arglist
    |   MapCall(m,arglist) -> "Called 'map' on arguments: " 
            ^ (List.fold_left (fun a ex -> a ^ string_of_expr(ex) ^ "; ") "" arglist)
            ^ " Using Mapper=" ^ string_of_mapper(m)
and  string_of_mapper = function
        NamedMap(name) -> name
    |   AnonyMap(list) -> "a statement list we can't evaluate yet"
and string_of_statement = function
        Expr(e) -> "Simple statement: " ^ string_of_expr(e)
    |   _ -> "Something not simple."
  
let string_of_program = function
    Content(l) -> "Statements in this program:\n" 
        ^ List.fold_left (fun s x -> s ^ string_of_statement( x ) ^ "\n" ) "" l