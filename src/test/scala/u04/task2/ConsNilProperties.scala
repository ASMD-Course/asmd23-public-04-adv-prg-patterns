package scala.u04.task2

import org.scalacheck.Gen

import scala.u04.task2.generator.SequenceGenerator
import scala.u04.task2.properties.SequenceProperties

object ConsNilProperties extends SequenceProperties("ConsNil") with SequenceGenerator:
  import scala.u04.task2.SequenceConsNil.*

  override def generatorOf[A](g: Gen[A]): Gen[SequenceADT[A]] = Gen.listOf(g).map(xs => apply(xs*))
  override def createEmpty[A]: SequenceADT[A] = empty[A]
  override def of[A](elements: A*): SequenceADT[A] = apply(elements*)
