package scala.u04.task3.property
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import u04.monads.Monads.Monad
import org.scalacheck.Arbitrary.arbitrary
import u04.datastructures.Streams.Stream.{Cons, Empty}

abstract class MonadicProperties[M[_]: Monad](name: String) extends Properties(name):

  import scala.u04.task3.generator.MonadGenerator.{given, *}
  import u04.datastructures.Streams.*
  import Monad.*
  
  //flatMap(unit(a), f) = f(a)
  property("flatMap unit") =
    forAll(arbitrary[Int], arbitrary[Int => M[Int]]):
      (a: Int, f: Int => M[Int]) =>
        summon[Monad[M]].unit(a).flatMap(f) == f(a)

  //flatMap(m, a => unit(a)) = m
  property("flatMap unit identity") =
    forAll(monadGenerator[M, Int]):
      (m: M[Int]) => m.flatMap(summon[Monad[M]].unit) == m

  //flatMap(flatMap(m, f), g) = flatMap(m, a => flatMap(f(a), g))
  property("flatMap associativity") =
    forAll(monadGenerator[M, Int], arbitrary[Int => M[Int]], arbitrary[Int => M[Int]]):
      (m: M[Int], f: Int => M[Int], g: Int => M[Int]) =>
        m.flatMap(f).flatMap(g) == m.flatMap(a => f(a).flatMap(g))


  //map2(unit(a), unit(b), f) = unit(f(a, b))
  property("map2 unit") =
    forAll(arbitrary[Int], arbitrary[Int], arbitrary[(Int, Int) => Int]):
      (a: Int, b: Int, f: (Int, Int) => Int) =>
        map2(summon[Monad[M]].unit(a), summon[Monad[M]].unit(b))(f) == summon[Monad[M]].unit(f(a, b))

  //map2(m1, m2, (a, b) => b) = seq(m1, m2)
  property("map2 seq") =
    forAll(monadGenerator[M, Int], monadGenerator[M, Int]):
      (m1: M[Int], m2: M[Int]) =>
        map2(m1, m2)((_, b) => b) == seq(m1, m2)

  // seqN(Stream(m)) = m
  property("seqN single element") =
    forAll(monadGenerator[M, Int]):
      (m: M[Int]) =>
        val singleStream = Stream.cons(m, Empty())
        seqN(singleStream) == m

  // seqN(Stream(m, ms)) = seq(m, seqN(Stream(ms)))
  property("seqN multiple elements") =
    forAll(monadGenerator[M, Int], arbitrary[Stream[M[Int]]].suchThat(_ != Empty())):
      (m: M[Int], ms: Stream[M[Int]]) =>
        val stream = Stream.cons(m, ms)
        seqN(stream) == seq(m, seqN(ms))

















