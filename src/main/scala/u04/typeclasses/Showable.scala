package u04.typeclasses
import u04.datastructures.Sequences.*

import scala.::

object Showables:
  // the Showable type class  
  trait Showable[T]:
    def show(t: T): String

  // algorithms/operations on showables
  object Showable:

    extension [A: Showable](a: A) 
      def show(): String = summon[Showable[A]].show(a)

    def showPair[A: Showable, B: Showable](t: (A, B)): String = t match
      case (a, b) => "(" + a.show() + ", " + b.show() + ")"

    def showSequence[A: Showable](seq: Sequence[A]): String = seq match
      case Sequence.Cons(h, t) => "|" + h.show() + showSequence(t)
      case Sequence.Nil() => ":"

object ShowableGivenInstances:      
  import Showables.*, Showable.* 

  // canonical terms for Int, String, Student
  given Showable[Int] with 
    def show(i: Int): String = "" + i
  
  given Showable[String] with
    def show(s: String): String = s

  case class Student(name: String, id: Int)

  given Showable[Student] with
    def show(s: Student): String = s match
      case Student(n, i) => "stud("+ n.show() + ", " + i.show()+")"

  given [A: Showable]: Showable[List[A]] with
    def show(lst: List[A]): String =
      lst.map(summon[Showable[A]].show).mkString("[", ", ", "]")

@main def tryShowable =
  import Showables.*, Showable.*
  import ShowableGivenInstances.{*, given}
  
  // note it seems like we have actually dynamically extended Int, String...
  println(10.show())
  println("hello!".show())
  println(Student("mario", 201).show())
  println(("a"::"b"::"c"::List()).show())
