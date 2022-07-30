package bscript

import bscript.Lexer.Token
object implicits:
    given CharListToString: Conversion[List[Char], String] with
            def apply(chars: List[Char]): String =
            chars.mkString

    given StringToCharList: Conversion[String, List[Char]] with
        def apply(string: String): List[Char] =
            string.toCharArray.toList

    given CharParseable: Parseable[Char] with
            extension (char: Char)
                def eq(that: Char): Boolean = char == that
                def show: String = char.toString()

    given TokenParseable : Parseable[Token] with
        extension (t1: Token)
            def eq (that: Token): Boolean = t1.equals(that)
            def show: String = t1.toString()
end implicits