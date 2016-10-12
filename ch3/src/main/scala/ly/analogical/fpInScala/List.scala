package ly.analogical.fpInScala

import scala.annotation.tailrec

/**
  * Created by Paul Oglesby on 26/04/2016.
  * Base taken from fit
  */
sealed trait List[+A] {

  import List._

  /**
    * Ex 3.2
    * Implement the function `tail` for removing the first element of a list. Note that the function takes constant time.
    * What are the different choices you could make in your implementation if the `List` is `Nil`?
    * Hint: start with thinking about data sharing in functional data structures...
    */

  def tail: List[A] = this match {
    case Cons(_, xs) => xs
    case _ => Nil // this is a subjective design choice; could equally return an error - probably better (safer) practice to do so!
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

  /**
    * Ex 3.5
    * Implement dropWhile, which removes elements from the List prefix as long as they match a predicate.
    */

  def dropWhile(p: A => Boolean): List[A] = {
    @tailrec
    def loop(ys: List[A], acc: List[A]): List[A] = ys match {
      case Nil => acc
      case Cons(head, tail) => if (!p(head)) loop(tail, Cons(head, acc)) else loop(tail, acc)
    }
    loop(this, Nil)
  }

  /**
    * Ex 3.6
    * Not everything works out so nicely. Implement a function, init, that returns a List consisting of all but the last element of a List.
    * So, given List(1,2,3,4), init will return List(1,2,3).
    * Why can’t this function be implemented in constant time like tail?
    *
    * ... this one is interesting! 2 things show up immediately from writing the code:
    *   1. the append-bias implementation of the List data structure jumps out! It is easy to append to a List, but this method needs us to recurse through it
    *   2. our recursion stops when we match on a Nil...i.e. at the end of the List. Recursing back from there with appends (all we have) means we reverse the original list...
    *   ... so we need to reverse it again before returning the result!
    *
    * reverse is probably useful in many situations because of this directional bias, so move it to the base object!
    */
  def init: List[A] = {
    @tailrec
    def loop(ys: List[A], acc: List[A] = Nil): List[A] = ys match {
      case Cons(h, Nil) => acc
      case Nil => acc
      case Cons(h, _) => loop(ys.tail, Cons(h, acc))
    }
    reverse(loop(this))
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

  @tailrec
  def reverse[A](xs: List[A], acc: List[A] = Nil): List[A] = xs match {
    case Cons(h, _) => reverse(xs.tail, Cons(h, acc))
    case _ => acc
  }

}

