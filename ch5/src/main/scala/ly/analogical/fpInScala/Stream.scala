package ly.analogical.fpInScala

import scala.annotation.tailrec

sealed trait Stream[+A] {

  import Stream._

  // NB: Explicit forcing of the h thunk using h()
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  /**
    * Ex 5.1
    * Write a function to convert a Stream to a scala collection `List`,
    * which will force its evaluation and let you look at it in the REPL.
    */
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  /**
    * Ex 5.2
    * Write the function `take(n)` for returning the first n elements of a Stream,
    * and `drop(n)` for skipping the first n elements of a Stream.
    */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  /** Ex 5.3
    * Write the function `takeWhile` for returning all starting elements of a Stream that match the given predicate.
    */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case Cons(h, t) if !p(h()) => t().takeWhile(p)
    case _ => this
  }

  /**
    * Ex 5.4
    * Implement forAll, which checks that all elements in the Stream match a given predicate.
    * Your implementation should terminate the traversal as soon as it encounters a nonmatching value.
    */
  def forAllNoFold(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAllNoFold(p)  // NB: && is non-strict in its second argument; first `p(h()) == false` terminates
    case _ => true
  }

  /**
    * Non-strict `foldRight`
    * Note that `f` takes its second argument by name and may choose not to evaluate it.
    */
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAll(p: A => Boolean): Boolean = this.foldRight(true)((a, b) => p(a) && b)   // NB && non-strict, AND b is tail of stream (also non-strict)
  def exists(p: A => Boolean): Boolean = this.foldRight(false)((a, b) => p(a) || b)  // Again, || non-strict - just takes one p(a) == true to terminate

  /**
    * Ex 5.5
    * Use `foldRight` to implement `takeWhile`.
    */
  def takeWhileFromFoldRight(p: A => Boolean): Stream[A] =
    this.foldRight[Stream[A]](Empty)((a, as) => if (p(a)) cons(a, as) else as)

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    // cache evaluated thunks as lazy vals!
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}
