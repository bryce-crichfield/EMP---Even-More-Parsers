package bscript

import scala.language.postfixOps
import StandardParser.{character as c, string as s, letter, digit, alphanumeric}
import implicits.{StringToCharList, CharParseable, CharListToString, TokenParseable}


object Lexer:

    // Token ADT
    trait Token

    trait Symbol extends Token:
        val value: String
        def parser: Parser[Char, Token] =
            s(value).map(_ => this)

    case class __LParen (value: String = "(")  extends Symbol
    case class __RParen (value: String = ")")  extends Symbol
    case class __LCurly (value: String = "{")  extends Symbol
    case class __RCurly (value: String = "}")  extends Symbol
    case class __Comma  (value: String = ",")  extends Symbol
    case class __Quote  (value: String = "\"") extends Symbol
    case class __Dot    (value: String = ".")  extends Symbol
    case class __Colon  (value: String = ":")  extends Symbol
    case class __Minus  (value: String = "-")  extends Symbol
    case class __Plus   (value: String = "+")  extends Symbol
    case class __Times  (value: String = "*")  extends Symbol
    case class __Divide (value: String = "/")  extends Symbol
    case class __And    (value: String = "&")  extends Symbol
    case class __Or     (value: String = "|")  extends Symbol
    case class __Lt     (value: String = "<")  extends Symbol
    case class __Gt     (value: String = ">")  extends Symbol
    case class __Assign (value: String = "=")  extends Symbol
    case class __Equiv  (value: String = "==") extends Symbol
    case class __Rocket (value: String = "=>") extends Symbol

    val LParen : __LParen = __LParen()
    val RParen : __RParen = __RParen()
    val LCurly : __LCurly = __LCurly()
    val RCurly : __RCurly = __RCurly()
    val Comma  : __Comma  = __Comma()
    val Quote  : __Quote  = __Quote()
    val Dot    : __Dot    = __Dot()
    val Colon  : __Colon  = __Colon()
    val Minus  : __Minus  = __Minus()
    val Plus   : __Plus   = __Plus()
    val Times  : __Times  = __Times()
    val Divide : __Divide = __Divide()
    val And    : __And    = __And()
    val Or     : __Or     = __Or()
    val Lt     : __Lt     = __Lt()
    val Gt     : __Gt     = __Gt()
    val Assign : __Assign = __Assign()
    val Equiv  : __Equiv  = __Equiv()
    val Rocket : __Rocket = __Rocket()


    case class __Fn(value: String = "fn") extends Symbol
    case class __If(value: String = "if") extends Symbol
    case class __Else(value: String = "else") extends Symbol
    case class __Return(value: String = "ret") extends Symbol

    val Fn: __Fn = __Fn()
    val If: __If = __If()
    val Else: __Else = __Else()
    val Return: __Return = __Return()


    case class __Newline (value: String = "\n") extends Symbol
    case class __Tab (value: String = "\t") extends Symbol
    case class __Space (value: String = " ") extends Symbol

    val Newline : __Newline = __Newline()
    val Tab : __Tab = __Tab()
    val Space : __Space = __Space()


    def Glyph = {
        def braces = LParen.parser | RParen.parser  | LCurly.parser  | RCurly.parser 
        def seps = Comma.parser | Quote.parser  | Dot.parser | Colon.parser
        def arith = Minus.parser | Plus.parser | Times.parser | Divide.parser
        def bool = And.parser | Or.parser | Lt.parser | Gt.parser
        def eq = Equiv.parser | Rocket.parser |Assign.parser 
        braces | seps | arith |bool | eq
    }
    def Keyword = Fn.parser | If.parser | Else.parser
    def Whitespace = Newline.parser | Tab.parser | Space.parser

    def sym[T <: Symbol](symbol: T): Parser[Token, T] = Parser { 
        (input: List[Token]) => input match
            case t::ts => t match
                case s: Symbol if s.value == symbol.value => 
                    Success(ts, symbol)
                case _ => Failure(input, "sym")
            case _ => Failure(input, "sym")
    }



    trait Data[A] extends Token:
        val _type: String
        val capture: A
        def parser: Parser[Char, Token] = {
            (letter & alphanumeric.*) map {
                case (c, l) => __Id((c::l).mkString)
            }
        }
        
    val Id : __Id = __Id("")
    case class __Id(capture: String, _type: String = "id") extends Data[String]:
        override def parser: Parser[Char, Token] = {
            (letter & alphanumeric.*) map {
                case (c, l) => __Id((c::l).mkString)
            }
        }
    end __Id
    
    val StringLiteral : __StringLiteral = __StringLiteral("")
    case class __StringLiteral(capture: String, _type: String = "str") extends Data[String] :
        override def parser: Parser[Char, Token] = {
            def parsed: Parser[Char, List[Char]] = 
                Quote.parser &> (StandardParser.all.*) <& Quote.parser
            parsed map (s => __StringLiteral(s.mkString))
        }
    end __StringLiteral

    val NumericLiteral : __NumericLiteral = __NumericLiteral(0)
    case class __NumericLiteral(capture: Int, _type: String = "num") extends Data[Int]:
        override def parser: Parser[Char, Token] = {
            def p1 = digit.*
            p1 map (l => __NumericLiteral(Integer.parseInt(l.mkString)))

        }
    // Right Now This Can Only Capture Doubles and its really convoluted
        
    def DataLiteral = Id.parser | StringLiteral.parser
    
    def dat[A <: Data[_]](archetype: A): Parser[Token, A] = Parser {
        (in: List[Token]) => in match
            case t::ts => t match
                case d: Data[_] if d._type == archetype._type => 
                    Success(ts, d.asInstanceOf[A])
                case _ => Failure(in, "")
            case _ => Failure(in, "")
    }

    def lex: Parser[Char, List[Token]] =
        // NOTE: The order in which these parsers are applied defines precedence
        // val p = StringLiteral.parser | Keyword | Glyph | Whitespace | Id.parser | NumericLiteral.parser
        val p = StringLiteral.parser | Keyword | Glyph | Whitespace | Id.parser
        p.*
end Lexer