package ly.analogical.fpInScala

import scala.annotation.tailrec

/**
  * Created by Paul Oglesby on 11/03/2016.
  */
object Sorted {

  /**
    * Ex 2.2: Implement polymorphic isSorted[A], which checks whether an Array[A] is sorted
    * according to a given comparison function
    */

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(as: List[A], prevOrdered: Boolean): Boolean = {
      if (!prevOrdered) false else {
        as match {
          case Nil => prevOrdered
          case head :: Nil => prevOrdered
          case head :: tail => loop(tail, ordered(head, tail.head))
        }
      }
    }
    loop(as.toList, true)
  }

}
