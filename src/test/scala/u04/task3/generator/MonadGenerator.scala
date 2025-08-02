package scala.u04.task3.generator

import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.scalacheck.Arbitrary.arbitrary
import u04.datastructures.Streams.Stream.Empty
import u04.monads.Monads.Monad


case object MonadGenerator:
  def monadGenerator[M[_]: Monad, A: Arbitrary]: Gen[M[A]] = for
    el <- arbitrary[A]
    m <- summon[Monad[M]].unit(el)
  yield m

  import u04.datastructures.Streams.*

  given[A: Arbitrary]: Arbitrary[Stream[A]] =
    Arbitrary:
      Gen.oneOf(
        Gen.const(Empty()),
        for 
          head <- arbitrary[A]
          tail <- arbitrary[Stream[A]]
        yield Stream.cons(head, tail)
      )

  given [M[_]: Monad, A: Arbitrary]: Arbitrary[Stream[M[A]]] =
    Arbitrary:
      Gen.oneOf(
        Gen.const(Empty()),
        for
          head <- monadGenerator[M, A]
          tail <- arbitrary[Stream[M[A]]]
        yield Stream.cons(head, tail)
      )
  
  given [M[_] : Monad, A: Arbitrary](using Cogen[A]): Arbitrary[A => M[A]] =
    Arbitrary(Gen.function1(monadGenerator[M, A]))

@main
def testMonadGenerator(): Unit =
  import u04.monads.Optionals.{given, *}
  import scala.u04.task3.generator.MonadGenerator.monadGenerator

  val optionGen = monadGenerator[Optional, Int]
  println(optionGen.sample)