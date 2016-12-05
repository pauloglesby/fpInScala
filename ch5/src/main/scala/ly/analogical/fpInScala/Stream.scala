package ly.analogical.fpInScala

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
    case _ => empty[A]
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
    foldRight(empty[A])((a, as) => if (p(a)) cons(a, as) else empty[A])

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

  /**
    * Ex 5.13
    * Use `unfold` to implement `map`, `take`, `takeWhile`, `zipWith` (as in chapter 3), and `zipAll`.
    * The `zipAll` function should continue the traversal as long as either stream has more elements,
    * using `Option` to indicate whether each stream has been exhausted.
    */
  def mapFromUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  def takeFromUnfold(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h, t), k) if k > 0 => Some((h(), (t(), k - 1)))
    case _ => None
  }

  def takeWhileFromUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }

  def zipWith[B, C](sb: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, sb)) {
    case (Cons(ha, ta), Cons(hb, tb)) => Some(f(ha(), hb()), (ta(), tb()))
    case _ => None
  }

  def zipAll[B](sb: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, sb)) {
    case (Cons(ha, ta), Cons(hb, tb)) => Some((Some(ha()), Some(hb())), (ta(), tb()))
    case (Cons(ha, ta), _) => Some((Some(ha()), None), (ta(), empty[B]))
    case (_, Cons(hb, tb)) => Some((None, Some(hb())), (empty[A], tb()))
    case _ => None
  }

  /**
    * BOOM
    * No need to have pair of streams as state; we're streaming over the complete sb from every "initial" element
    * Use `zipAll` inside of `unfold` instead of `zipWith`
    * Hint: need both a and b available to ensure complete subsequence match
    * BUT: if we `takeWhile` on a *and* b defined, we risk truncating the subsequence and returning a false match at the end of the sequence
    */
  def hasSubsequence[A](sb: Stream[A]): Boolean = unfold(this) {
    case Cons(ha, ta) =>
      val check = {
        // `cons(ha, ta())` won't work due to non-strictness; use `cons(ha(), ta())` instead
        val sa = cons(ha(), ta())
        sa.zipAll(sb)
          .takeWhile { case (_, ob) => ob.isDefined }
          .foldRight(true) { case ((oa, ob), equal) =>
            {
              for {
                a <- oa
                b <- ob
              } yield a == b
            }.getOrElse(false) && equal
          }
      }
      Some(check, ta())
    case _ => None
  }.exists(_ == true)

  /**
    * Ex 5.14
    * Hard: Implement `startsWith` using functions you’ve written.
    * It should check if one Stream is a prefix of another. For instance, Stream(1,2,3) startsWith Stream(1,2) would be true.
    */
  def startsWith[A](s2: Stream[A]): Boolean = this match {
    case Empty => false
    case _ =>
      zipAll(s2)
        .takeWhile { case (_, o2) => o2.isDefined }
          .foldRight(true) { case ((o1, o2), equal) =>
            {
              for {
                v1 <- o1
                v2 <- o2.orElse(o1)
              } yield v1 == v2
            }.getOrElse(false) && equal
          }
  }

  /**
    * Ex 5.15
    * Implement tails using unfold.
    * For a given Stream, tails returns the Stream of suffixes of the input sequence, starting with the original Stream.
    * For example, given Stream(1,2,3), it would return Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream()).
    */
  def tails: Stream[Stream[A]] = unfold[Stream[A], Option[Stream[A]]](Some(this)) {
    case Some(Cons(h, t)) => Some((cons(h(), t()), Some(t())))
    case Some(Empty) => Some((empty[A], None))
    case _ => None
  }

  def hasSubsequenceViaTails[A](s: Stream[A]): Boolean = tails.exists(_ startsWith s)

  /**
    * Ex 5.16
    * Hard: Generalize tails to the function scanRight, which is like a foldRight that returns a stream of the intermediate results. For example:
    *    scala> Stream(1, 2, 3).scanRight(0)(_ + _).toList
    *    res0: List[Int] = List(6, 5, 3, 0)
    * This example should be equivalent to the expression List(1 + 2 + 3 + 0, 2 + 3 + 0, 3 + 0, 0).
    * Your function should reuse intermediate results so that traversing a Stream with n elements always takes time linear in n.
    * Can it be implemented using unfold? How, or why not? Could it be implemented using another function we’ve written?
    */
  def scanRightViaUnfold[B](z: B)(f: (A, => B) => B): Stream[B] = unfold[B, Option[Stream[A]]](Some(this)) {
    case Some(Cons(h, t)) => Some((f(h(), t().foldRight(z)(f)), Some(t())))
    case Some(Empty) => Some((z, None))
    case _ => None
  }

  /**
    * From the solutions...
    * The function can't be implemented using `unfold`, since `unfold` generates elements of the `Stream` from left to right.
    * It can be implemented using `foldRight` though.
    * The implementation is just a `foldRight` that keeps the accumulated value and the stream of intermediate results,
    * which we `cons` onto during each iteration.
    * When writing folds, it's common to have more state in the fold than is needed to compute the result.
    * Here, we simply extract the accumulated list once finished.
    */
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z))) { case (a, (b1, s)) =>
      {
        // a is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
        lazy val head = b1
        lazy val tail = s
        val b2 = f(a, head)
        (b2, cons(b2, tail))
      }
    }._2


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
