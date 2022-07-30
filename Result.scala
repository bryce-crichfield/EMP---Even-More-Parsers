package bscript

trait Result[+I, +O]:
    def onSuccess[B](t: O => B): Option[B] =
        this match 
            case Success(i, o) => Some(t(o))
            case f: Failure[I] => None

         
case class Success[I, O](i: I, o: O) extends Result[I, O]
object Success:
    def tuple[I, O](t: (I, O)): Success[I, O] = Success(t._1, t._2)
case class Failure[I](i: I, l: List[String]) extends Result[I, Nothing]:
    def + (msg: String): Failure[I] = this.copy(l = l:+msg)
object Failure:
    def apply[I](i: I, s: String): Failure[I] = Failure(i, List(s))