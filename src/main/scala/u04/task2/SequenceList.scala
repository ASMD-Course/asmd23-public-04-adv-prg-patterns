package scala.u04.task2

object SequenceList extends SequenceConstructor:

  private case class ListImplementation[A](elements: List[A]) extends SequenceADT[A]:
    
    override def add(element: A): SequenceADT[A] = ListImplementation(element :: elements)

    override def concat(other: SequenceADT[A]): SequenceADT[A] = ListImplementation(
      elements ++ other.asInstanceOf[ListImplementation[A]].elements
    )
    
    override def map[B](f: A => B): SequenceADT[B] = ListImplementation(elements.map(f))
  
    override def flatMap[B](f: A => SequenceADT[B]): SequenceADT[B] =
      val mapped: List[List[B]] = elements.map(f(_).asInstanceOf[ListImplementation[B]].elements)
      ListImplementation(mapped.flatten)
  
    override def filter(f: A => Boolean): SequenceADT[A] =
      ListImplementation(elements.filter(f))
  
    override def foldLeft[B](init: B)(f: (B, A) => B): B =
      elements.foldLeft(init)(f)

  
  override def apply[A](elements: A*): SequenceADT[A] = ListImplementation(elements.toList)

  override def empty[A]: SequenceADT[A] = ListImplementation(List.empty[A])
      
  
  

