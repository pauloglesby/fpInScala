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

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)   // NB && non-strict, AND b is tail of stream (also non-strict)
  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)  // Again, || non-strict - just takes one p(a) == true to terminate

  /**
    * Ex 5.5
    * Use `foldRight` to implement `takeWhile`.
    */
  def takeWhileFromFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, as) => if (p(a)) cons(a, as) else as)

  /**
    * Ex 5.6
    * Hard: Implement `headOption` using `foldRight`.
    */
  def headOptionFromFoldRight: Option[A] = foldRight[Option[A]](None)((a, _) => Some(a))  // NB tail of stream thrown away here because of non-strictness, enforced by not using it in the closure

  /**
    * Ex 5.7
    * Implement `map`, `filter`, `append`, and `flatMap` using `foldRight`.
    * The `append` method should be non-strict in its argument.
    */
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else t)

  def append[B >: A](s: Stream[B]): Stream[B] = foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h, t) => f(h).append(t))
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

  def ones: Stream[Int] = cons(1, ones)

  /**
    * Ex 5.8
    * Generalize ones slightly to the function constant, which returns an infinite Stream of a given value.
    */
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  /**
    * Ex 5.9
    * Write a function that generates an infinite stream of integers, starting from n, then n + 1, n + 2, and so on
    */
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  /**
    * Ex 5.10
    * Write a function fibs that generates the infinite stream of Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on.
    */
  def fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = cons(a, go(b, a + b))
    go(0, 1)
  }

  /**
    * Ex 5.11
    * Write a more general stream-building function called `unfold`.
    * It takes an initial state, and a function for producing both the next state and the next value in the generated stream.
    */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty[A]
  }

  /**
    * Ex 5.12
    * Write `fibs`, `from`, `constant`, and `ones` in terms of `unfold`.
    */
  def onesFromUnfold: Stream[Int] = unfold(1)(_ => Some(1, 1))
  def constantFromUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))
  def fromFromUnfold(n: Int): Stream[Int] = unfold(n)(k => Some(k, k + 1))
  def fibsFromUnfold: Stream[Int] = unfold[Int, (Int, Int)]((0, 1)) { case (a: Int, b: Int) => Some((a, (b, a + b))) }

}
