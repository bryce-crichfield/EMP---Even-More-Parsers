import scala.language.postfixOps
import bscript.StandardParser._
import bscript.Parser._
import bscript.implicits.StringToCharList
import bscript.Lexer.{Id, lex}
import bscript.{ExpressionParser, CommaList, FunctionDefinitionParser}
import bscript.Lexer.DataLiteral
import bscript.Lexer.{Newline, Tab, Space}
import bscript.FunctionDefinition

object Main extends App:
    val raw =  "a, b, c"
    def tokens_no_space = (in: List[Char]) => lex.map{ l =>
        l.filter { t =>
            t match
                case Newline | Tab | Space => false
                case _ => true
        }
    }(in)
    val tokenized = tokens_no_space(raw)
    tokenized.onSuccess(s => s.foreach(println))
    val parsed = tokenized.onSuccess(t =>  CommaList(Id)(t))
    parsed.foreach(println)



end Main

