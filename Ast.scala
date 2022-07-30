package bscript

import Lexer._
import bscript.Lexer
import implicits.{TokenParseable}

// Allows us to effectively lift a token type to a parser type in 
// the token domain
// Fails if the input is empty (and this case arrives)
// def syn[T <: Token] : Parser[Token, T] = Parser {
//     (in: List[Token]) => in match
//         case t1::ts => t1 match
//             case t2: T => Success(ts, t2)
//             case _ => Failure(in, List("Token Type Doesn't match"))
//         case Nil => Failure(Nil, List("Input Empty"))
//         case _ => Failure(in, List(s"Expected Token Type found otherwise"))
// }

// def sym[T <: SymbolToken](s: T): Parser[Token, T] = Parser {
//     (in: List[Token]) => in match
//         case t::ts => t match
//             case st: SymbolToken if st.symbol == s.symbol => Success(ts, s)
//             case _ => Failure(in, List(s"Expected ${s.symbol} found otherwise"))
//         case _ => Failure(in, List("sym : Input Error"))
// }

// ugly runtime type checking, cannot figure out how to avoid rn
// don't you see, the reason this is happening is because you are now
// in the process of type checking the script (ofc its runtime)
// def dat(input: String): Parser[Token, DataToken[_]] = Parser {
//     (in: List[Token]) => in match
//         case t::ts => t match
//             case dt: DataToken[_] if dt._type == input => Success(ts, dt)
//             case _ => Failure(in, List(s"Expected ${input} found otherwise"))
//         case _ => Failure(in, List("sym : Input Error"))
// }

case class Script (
    expressions: List[Expression]
)

trait Expression
def ExpressionParser: Parser[Token, Expression] = {
    FunctionDefinitionParser 
}

case class FunctionDefinition (
    __name: __Id, 
    __args: List[__Id], 
    __body: List[__Id]
) extends Expression

def FunctionDefinitionParser: Parser[Token, Expression] = {
    def head = dat(Id) <& sym(Assign)
    def args = sym(LParen) &> Parser.sep(dat(Id))(sym(Comma)) <& sym(RParen)
    head & args map {case (a, l) => FunctionDefinition(a, l, Nil)}
    // def body = syn[LCurly.type] &> syn[Id] <& syn[RCurly.type]
    // head & args & body map {case ((a, l), b) => FunctionDefinition(a, l, List(b))}
}

case class FunctionCall (
    __name: __Id,
    __args: List[Lexer.__StringLiteral]
) extends Expression

// def FunctionCallParser: Parser[Token, Expression] = {
//     def parsed = syn[Lexer.Id] & (syn[LParen.type] &> CommaList[StringLiteral] <& syn[RParen.type])
//     parsed map {
//         case (i, o) => FunctionCall(i, o)
//     }
// }

// case class VarAssignment(__name: Id, value: DataToken[_]) extends Expression


// I think the backing function in Parse, is failing because of the use of the `syn` function here
// `syn` relies on runtime type-parameter checks which fail due to type erasure
// `syn` should not rely on constructions via its type-constructor
// replace `syn` with typeclass mechanism - :::B:C:::7:15:::23:54:::
def CommaList[A <: Lexer.Data[_]](archetype: A) = Parser {
    (input: List[Token]) => input match
        case 
}

// def ArgList = syn[LParen.type] &> CommaList[Id] <& syn[RParen.type]



