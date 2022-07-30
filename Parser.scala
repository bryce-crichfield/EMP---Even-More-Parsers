package bscript

import scala.language.postfixOps

import implicits.{StringToCharList, CharParseable, CharListToString}

trait Parseable[T]:
    extension (t: T)
        def eq(that: T): Boolean
        def show: String

trait Parser[I: Parseable, O]:

    // ------------------------------------------
    def apply(i: List[I]): Result[List[I], O] 
    // ------------------------------------------


    // ------------------------------------------
    def map [B](t: O => B): Parser[I, B] = Parser {
        (in: List[I]) => this.apply(in) match
            case Success(i, o) => Success(i, t(o))
            case f: Failure[I] => f + "Map"
    }
    // ------------------------------------------


    // ------------------------------------------
    def flatMap [B](t: O => Parser[I, B]): Parser[I, B] = Parser {
        (in: List[I]) => this.apply(in) match
            case Success(i, o) => t(o)(i)
            case f: Failure[I] => f + "FlatMap"
    }
    // ------------------------------------------


    // Union ------------------------------------
    def & [B](that: Parser[I, B]): Parser[I, (O, B)] = Parser {
        (in: List[I]) => this.apply(in) match
            case Success(i1, o1) => that(i1) match
                case Success(i2, o2) => Success(i2, (o1, o2))
                case f: Failure[I] => f + "And 1st Case"
            case f: Failure[I] => f + "And 2nd Case"
    }
    // ------------------------------------------
    def <& [B] (that: Parser[I, B]): Parser[I, O] =
        (this & that) map { case (a, b) => a }
    // ------------------------------------------
    def &> [B] (that: Parser[I, B]): Parser[I, B] =
        (this & that) map { case (a, b) => b }
    // ------------------------------------------


    // Intersection -----------------------------
    def | [B >: O](that: Parser[I, B]): Parser[I, B] = Parser {
        (in: List[I]) => this.apply(in) match
            case Success(i1, o1) => Success(i1, o1)
            case f1: Failure[I] => that(in) match
                case Success(i2, o2) => Success(i2, o2)
                case f2: Failure[I] => Failure(in, f1.l++f2.l) + "Intersection"
    }
    // ------------------------------------------


    // Recurse ----------------------------------
    private def rec(in: List[I]): (List[I], List[O]) =
        this.apply(in) match
            case _: Failure[I] => (in, Nil)
            case Success(i, o) =>
                // Todo: We should resolve this empty list error in the entry point
                // The entry point being `Parser.matched`
                // Depending on the caller we might encounter an empty list for i
                // We should catch these and return
                scala.util.Try(rec(i)) match 
                    case scala.util.Success((t)) => (t._1, o::t._2)
                    case scala.util.Failure(e) => (i, List(o))
    // ------------------------------------------


    // Bounded Recurse --------------------------
    private def recd(in: List[I], d: Int): (List[I], List[O]) =
        if d == 0 then (in, Nil)
        else this.apply(in) match
            case _: Failure[I] => (in, Nil)
            case Success(i, o) =>
                // Depending on the caller we might encounter an empty list for i
                // We should catch these and return
                scala.util.Try(recd(i, d-1)) match 
                    case scala.util.Success((t)) => (t._1, o::t._2)
                    case scala.util.Failure(e) => (i, List(o))
    // ------------------------------------------

    // Zero or More -----------------------------
    def * : Parser[I, List[O]] = Parser {
        (in: List[I]) => Success.tuple(rec(in))
    }
    // ------------------------------------------

    
    // One or More ------------------------------
    def + : Parser[I, List[O]] = Parser {
        (in: List[I]) => this.apply(in) match
            case Success(i, o) =>
                val (ir, or) = rec(i)
                Success(ir, o::or)
    }
    // ------------------------------------------ 

    // Reps up to n -----------------------------
    def #* (n: Int) : Parser[I, List[O]] = Parser {
        (in: List[I]) => 
            val (i, o) = recd(in, n)    
            Success(i, o)
    }
    // ------------------------------------------  

    // Reps n -----------------------------------
    def #= (n: Int) : Parser[I, List[O]] = Parser {
        (in: List[I]) =>
            val (i, o) = recd(in, n)
            if o.length != n then
                Failure(i, List(s"Expeced: ${n}, but only found ${o.length}"))
            else Success(i, o)
    }
    // ------------------------------------------ 

            
end Parser

object Parser:
    def apply[I: Parseable, O](f: List[I] => Result[List[I], O]) : Parser[I, O] =
        new Parser {
            override def apply(i: List[I]): Result[List[I], O] = f(i)
        }


    def fail [I: Parseable, O] (msg: String) : Parser[I, O] = Parser {
        (in: List[I]) => Failure(in, List(msg))
    }
    // ------------------------------------------ 
    // Applicative 
    extension [I: Parseable, O1, O2] (pf: Parser[I, O1 => O2])
        def use(pa: Parser[I, O1]): Parser[I, O2] = 
            (pf.&(pa)).map { case (f, o2) => f(o2) }
    // ------------------------------------------ 
    def pure [I: Parseable, O] (out: O) : Parser[I, O] = Parser {
        (in: List[I]) => Success(in, out)
    }
    // ------------------------------------------ 
    def lift [I: Parseable, O1, O2, O3] (f: O1 => O2 => O3) 
      (po1: Parser[I, O1]) (po2: Parser[I, O2]) : Parser[I, O3] =
        pure(f).use(po1).use(po2)
    // ------------------------------------------ 
    def seq [I: Parseable, O] (ps: List[Parser[I, O]]) : Parser[I, List[O]] =
        def apnd(o: O)(os: List[O]): List[O] = o::os
        val apndp : Parser[I, O] => Parser[I, List[O]] => Parser[I, List[O]] =
            lift(apnd)
        ps match
            case (h::t) => apndp(h)(seq(t))
            case _ => pure(Nil)
    // ------------------------------------------ 
    def ? [I: Parseable, O](pa: Parser[I, O]): Parser[I, Option[O]] =
        val some: Parser[I, Option[O]] = pa map { a => Some(a)  }
        val none: Parser[I, Option[O]] = pure(None)
        some | none
    // ------------------------------------------  
    def anyof [I] (choices: List[I]) (selector: I => Parser[I, I]) : Parser[I, I] = 
        val ps = choices.map(selector)
        ps.reduce { case (acc, p) => acc | p}
    // ------------------------------------------  
    
    // Generic Seperated List of Form -----------
    // list ::= o & (o s) & o
    // Returns:
    // Nil -> No List Found
    // List[...] -> List Found Without Seperator
    def sep[I: Parseable, O, S]
        (itm: Parser[I, O])(sep: Parser[I, S])
    : Parser[I, List[O]] = 
        def _s1 = itm map (o => List(o)) 
        def _s2 = (sep &> itm).*  
        _s2 | _s1

        // def head = Parser.?(pd)
        // def tail = sep &> pd
        // (head & (tail.*) map { 
        //     case (i, is) => if i == None then None else Some(i.get::is)
        // })  map { l => l.getOrElse(Nil) }
    
    // ------------------------------------------  
    def matched [T: Parseable] (t: T): Parser[T, T] = Parser {
        (in: List[T]) => in match
            case th::Nil if t.eq(th) => Success(Nil, th)
            case th::tt  if t.eq(th) => Success(tt, th)
            case Nil => Failure(Nil, List(s"Target : Input empty can not match ${t.show}"))
            case _ => Failure(in, List(s"Target : Expected ${t.show} found otherwise"))
    }
end Parser



object StandardParser:
    def character = Parser.matched[Char]

    def string (target: List[Char]) : Parser[Char, List[Char]] =
        Parser.seq(target map(character)) 

    def lowercase: Parser[Char, Char] =
        Parser.anyof("abcdefghijklmnopqrstuvwxyz")(character)

    def uppercase: Parser[Char, Char] =
        Parser.anyof("ABCDEFGHIJKLMOPQRSTUVWXYZ")(character)

    def digit: Parser[Char, Char] =
        Parser.anyof("1234567890")(character)

    def letter: Parser[Char, Char] =
        lowercase | uppercase
    
    def alphanumeric: Parser[Char, Char] =
        digit | letter

    // Note: Currently doesn't account for quotes
    def all: Parser[Char, Char] = 
        Parser.anyof(" !#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")(character)
end StandardParser
