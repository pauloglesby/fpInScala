package ly.analogical.fpInScala

import scala.annotation.tailrec

/**
  * Created by Paul Oglesby on 26/04/2016.
  * Base taken from fit
  */
sealed trait List[+A] {

  /**
    * Ex 3.2
    * Implement the function `tail` for removing the first element of a list. Note that the function takes constant time.
    * What are the different choices you could make in your implementation if the `List` is `Nil`?
    * Hint: start with thinking about data sharing in functional data structures...
    */

  def tail: List[A] = this match {
    case Cons(_, xs) => xs
    case _ => Nil
  }

  def head: Option[A] = this match {
    case Cons(x, _) => Some(x)
    case _ => None
  }

  /**
    * Ex 3.3
    * Using the same idea, implement `setHead` for replacing the first element of a `List` with a different value.
    */

  def setHead[B >: A](z: B): List[B] = this match {
    case Cons(_, tail) => Cons(z, tail)
    case _ => Cons(z, Nil)
  }

  /**
    * Ex 3.4
    * Generalize `tail` to the function `drop`, which removes the first n elements from a `List`.
    * Note that this function takes time proportional to the number of elements being dropped -
    *  we don't need to make a copy of the entire list.
    */

  def drop(n: Int): List[A] = {
    @tailrec
    def loop(ys: List[A], m: Int): List[A] = {
      if (m == 0) ys
      else ys match {
        case Nil => Nil
        case Cons(_, tail) => loop(tail, m - 1)
      }
    }
    loop(this, n)
  }
}

case object Nil extends List[Nothing]
case class Cons[+A](x: A, xs: List[A]) extends List[A]

object List {

  def sum(xs: List[Int]): Int = xs match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(xs: List[Double]): Double = xs match {
    case Nil => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

}

