package scala.u04.task2

trait SequenceADT[A]:
  def add(element: A): SequenceADT[A]
  def concat(other: SequenceADT[A]): SequenceADT[A]
  def map[B](f: A => B): SequenceADT[B]
  def flatMap[B](f: A => SequenceADT[B]): SequenceADT[B]
  def filter(f: A => Boolean): SequenceADT[A]
  def foldLeft[B](init: B)(f: (B, A) => B): B

trait SequenceConstructor:
  def apply[A](elements: A*): SequenceADT[A]
  def empty[A]: SequenceADT[A]

