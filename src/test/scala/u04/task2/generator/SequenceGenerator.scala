package scala.u04.task2.generator

import org.scalacheck.{Arbitrary, Cogen, Gen}

import scala.u04.task2.SequenceADT

trait SequenceGenerator:
  def generatorOf[A](gen: Gen[A]): Gen[SequenceADT[A]]
  
  def createEmpty[A]: SequenceADT[A]
  
  def of[A](elements: A*): SequenceADT[A]

  
      
    
    
