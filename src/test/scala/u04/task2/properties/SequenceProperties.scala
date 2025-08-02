package scala.u04.task2.properties

import org.scalacheck.{Arbitrary, Cogen, Gen, Properties}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll

import scala.u04.task2.SequenceADT
import scala.u04.task2.generator.SequenceGenerator

abstract class SequenceProperties(name: String) extends Properties(name) :
  self: SequenceGenerator =>

  /**
   * ============
   * Add properties
   * ============
   */
  // add(empty, x) = apply(x)
  property("add empty") =
    forAll(arbitrary[Int]) : x =>
      createEmpty[Int].add(x) == of(x)

  // add(apply(xs), x) = apply(xs, x)
  property("add cons") =
    forAll(arbitrary[List[Int]], arbitrary[Int]) :
      (l: List[Int], x: Int) =>
        of(l*).add(x) == of(x :: l*)

  /**
   * ============
   * Concat properties
   * ============
   */
  // concat(empty, xs) = xs
  property("concat empty") =
    forAll(generatorOf[Int](Gen.choose(0, 100))) : xs =>
      createEmpty[Int].concat(xs) == xs

  // concat(xs, ys) = apply(xs, ys)
  property("concat cons") =
    forAll(arbitrary[List[Int]], arbitrary[List[Int]]) :
      (l1: List[Int], l2: List[Int]) =>
        of(l1*).concat(of(l2*)) == of(l1 ++ l2 *)

  /**
   * ============
   * Map properties
   * ============
   */
  // map(empty , f) = nil
  property("map empty") =
    forAll(arbitrary[Int => Int]) : f =>
     createEmpty[Int].map(f) == createEmpty[Int]

  // map(apply(xs), i => i) = apply(xs, f)
  property("map identity") =
    forAll(generatorOf[Int](Gen.choose(0, 100))) : seq =>
      seq.map((x: Int) => x) == seq

  // map(apply(x, y), f) = apply(f(x), f(y))
  property("map cons") =
    forAll(arbitrary[Int], arbitrary[Int], arbitrary[Int => Int]) :
      (x: Int, y: Int, f: Int => Int) =>
        of(x, y).map(f) == of(f(x), f(y))

  /**
   * ============
   * FlatMap properties
   * ============
   */
  given [A, B](using a: Arbitrary[B], cg: Cogen[A]): Arbitrary[A => SequenceADT[B]] =
    Arbitrary(Gen.function1(generatorOf(a.arbitrary)))

  // flatMap(empty, f) = empty
  property("flatMap empty") =
    forAll(arbitrary[Int => SequenceADT[Int]]) : f =>
      createEmpty[Int].flatMap(f) == createEmpty[Int]

  // flatMap(apply(x), f) = f(x)
  property("flatMap single element") =
    forAll(arbitrary[Int], arbitrary[Int => SequenceADT[Int]]) :
      (x: Int, f: Int => SequenceADT[Int]) =>
        of(x).flatMap(f) == f(x)

  // flatMap(apply(x1, x2, ..., xn), f) = concat(f(x1), flatMap(apply(x2, ..., xn), f))
  property("flatMap multiple elements") =
    forAll(arbitrary[List[Int]], arbitrary[Int => SequenceADT[Int]]) :
      (l: List[Int], f: Int => SequenceADT[Int]) =>
        if l.isEmpty then createEmpty[Int].flatMap(f) == createEmpty[Int]
        else of(l*).flatMap(f) == f(l.head).concat(of(l.tail*).flatMap(f))

  // flatMap(apply(x, y), f) = f(x) ++ f(y)
  property("flatMap cons") =
    forAll(arbitrary[Int], arbitrary[Int], arbitrary[Int => SequenceADT[Int]]) :
      (x: Int, y: Int, f: Int => SequenceADT[Int]) =>
        of(x, y).flatMap(f) == f(x).concat(f(y))

 /**
  * ============
  * Filter properties
  * ============
  */
  // filter(empty, f) = empty
  property("filter empty") =
    forAll(arbitrary[Int => Boolean]) : f =>
      createEmpty[Int].filter(f) == createEmpty[Int]

  // filter(apply(x1, x2, ..., xn), p) = concat(filter(apply(x1), p), filter(apply(x2, ..., xn), p))
  property("filter multiple elements") =
    forAll(arbitrary[List[Int]], arbitrary[Int => Boolean]) :
      (l: List[Int], f: Int => Boolean) =>
        if l.isEmpty then createEmpty[Int].filter(f) == createEmpty[Int]
        else of(l*).filter(f) == of(l.head).filter(f).concat(of(l.tail*).filter(f))

  // filter(apply(xs), f) = apply(xs.filter(f))
  property("filter cons") =
    forAll(arbitrary[List[Int]], arbitrary[Int => Boolean]) :
      (l: List[Int], f: Int => Boolean) =>
        of(l*).filter(f) == of(l.filter(f)*)

 /**
  * ============
  * FoldLeft properties
  * ============
  */
  // foldLeft(empty, b, f) = b
  property("foldLeft empty") =
    forAll(Gen.choose(-100, 100), arbitrary[(Int, Int) => Int]):
      (b, f) =>
        createEmpty[Int].foldLeft(b)(f) == b

  //foldLeft(apply(x, xs), b, f) = foldLeft(xs, f(b, x), f)
  property("foldLeft cons") =
    forAll(arbitrary[Int], arbitrary[List[Int]], arbitrary[(Int, Int) => Int]) :
      (x: Int, l: List[Int], f: (Int, Int) => Int) =>
        of(x :: l*).foldLeft(0)(f) == of(l*).foldLeft(f(0, x))(f)
  
  // foldLeft(concat(xs, ys), b, f) = foldLeft(ys, foldLeft(xs, b, f), f)
  property("foldLeft concat") =
    forAll(generatorOf[Int](Gen.choose(0, 100)), Gen.choose(-100, 100), arbitrary[(Int, Int) => Int]) :
      (xs: SequenceADT[Int], b: Int, f: (Int, Int) => Int) =>
        xs.concat(createEmpty[Int]).foldLeft(b)(f) == createEmpty[Int].foldLeft(xs.foldLeft(b)(f))(f)
  
  //foldLeft(apply(x1, x2, ..., xn), b, f) = f(f(...f(f(b, x1), x2)..., xn-1), xn)
  property("foldLeft multiple elements") =
    forAll(arbitrary[List[Int]], Gen.choose(-100, 100), arbitrary[(Int, Int) => Int]) :
      (l: List[Int], b: Int, f: (Int, Int) => Int) =>
        if l.isEmpty then createEmpty[Int].foldLeft(b)(f) == b
        else of(l*).foldLeft(b)(f) == l.foldLeft(b)(f)
  














