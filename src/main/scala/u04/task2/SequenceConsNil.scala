package scala.u04.task2


object SequenceConsNil extends SequenceConstructor:

  private enum SequenceImpl[A] extends SequenceADT[A]:
    case Cons(head: A, tail: SequenceImpl[A])
    case Nil()
    
    override def add(element: A): SequenceImpl[A] = Cons(element, this)
    
    override def concat(other: SequenceADT[A]): SequenceImpl[A] = this match
      case Nil() => other.asInstanceOf[SequenceImpl[A]]
      case Cons(h, t) => Cons(h, t.concat(other))
    
    override def map[B](f: A => B): SequenceImpl[B] = this match
      case Nil() => Nil()
      case Cons(h, t) => Cons(f(h), t.map(f))

    override def flatMap[B](f: A => SequenceADT[B]): SequenceImpl[B] =
      def concat(s1: SequenceImpl[B], s2: => SequenceImpl[B]): SequenceImpl[B] = s1 match
        case Nil() => s2
        case Cons(h, t) => Cons(h, concat(t, s2))

      this match
        case Nil() => Nil()
        case Cons(h, t) => concat(f(h).asInstanceOf[SequenceImpl[B]], t.flatMap(f))

    override def filter(f: A => Boolean): SequenceImpl[A] = this match
      case Nil() => Nil()
      case Cons(h, t) =>
        if f(h) then Cons(h, t.filter(f))
        else t.filter(f)

    override def foldLeft[B](init: B)(f: (B, A) => B): B = this match
      case Nil() => init
      case Cons(h, t) => t.foldLeft(f(init, h))(f)
    
  end SequenceImpl
  

  import SequenceImpl.*

  override def apply[A](elements: A*): SequenceADT[A] =
    elements.foldRight(Nil(): SequenceImpl[A])((elem, acc) => Cons(elem, acc))

  override def empty[A]: SequenceADT[A] = Nil()



