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
    * reverse is probably useful in many situations because of this directional bias, so move it to the trait
    */
  def init: List[A] = {
    @tailrec
    def loop(ys: List[A], acc: List[A] = Nil): List[A] = ys match {
      case Cons(h, Cons(_, _)) => loop(ys.tail, Cons(h, acc))
      case _ => acc
    }
    loop(this).reverse
  }

  def reverse: List[A] = {
    @tailrec
    def loop[A](xs: List[A], acc: List[A] = Nil): List[A] = xs match {
      case Cons(h, _) => loop(xs.tail, Cons(h, acc))
      case _ => acc
    }
    loop(this, Nil)
  }

  def foldRight[B](z: B)(f: (A, B) => B): B = this match {
    case Nil => z
    case Cons(h, t) => f(h, this.tail.foldRight(z)(f))
  }

  def foldRightShortCircuit[B](z: B)(f: (A, B) => B)(shortCircuit: A => Boolean)(shortCircuitDefault: B): (B, Int) = {
    @tailrec
    def loop(xs: List[A], acc: B, i: Int = 0): (B, Int) = xs match {
      case Nil => (acc, i)
      case Cons(h, t) if shortCircuit(h) => (shortCircuitDefault, i + 1)
      case Cons(h, t) => loop(t, f(h, acc), i + 1)
    }
    loop(this, z)
  }

  /**
    * Ex 3.9
    * Compute the length of a list using foldRight
    */
  def length: Int = foldRight(0)((e, l) => l + 1)

  /**
    * Ex 3.10
    * Our implementation of foldRight is not tail-recursive and will result in a StackOverflowError for large lists (we say it’s not stack-safe).
    * Convince yourself that this is the case, and then write another general list-recursion function, foldLeft,
    * that is tail-recursive, using the techniques we discussed in the previous chapter.
    *
    * NB: the difference between this implementation and the one given in the solution is because we've put the method onto the List instance.
    * The inner loop here is equivalent to the solution `foldLeft`; we've just wrapped it to consume `this`.
    */
  def foldLeft[B](z: B)(f: (B, A) => B): B = {
    @tailrec
    def loop(xs: List[A], acc: B): B = xs match {
      case Nil => acc
      case Cons(h, t) => loop(t, f(acc, h))
    }
    loop(this, z)
  }

  /**
    * Ex 3.12 write a function to reverse a list using a `fold`.
    */
  def reverse2: List[A] = foldLeft[List[A]](Nil)((acc, h) => Cons(h, acc))

  /**
    * Ex 3.13 (Hard)
    * Can you write foldLeft in terms of foldRight? How about the other way around?
    *
    * Implementing foldRight in terms of foldLeft is useful because it lets us implement foldRight tail-recursively.
    *
    */

  def foldRight2[B](z: B)(f: (A, B) => B): B = this.reverse2.foldLeft(z)((acc, h) => f(h, acc))

  /**
    * Ex 3.14
    * Implement append in terms of either foldLeft or foldRight
    *
    */
  def append[B >: A](b: B): List[B] = foldRight2(Cons(b, Nil))((h, acc) => Cons(h, acc))

  /**
    * Ex 3.18
    * Write a function `map` that generalizes modifying each element whilst retaining the structure of the list.
    * See also `increment` and `doublesAsStrings`, below.
    */
  def map[B](f: A => B): List[B] = foldLeft[List[B]](Nil)((xs, x) => xs.append(f(x)))

  /**
    * Ex 3.19
    * Write a function filter that removes elements from a list unless they satisfy a given predicate.
    * Use it to remove all odd numbers from a List[Int]
    */
  def filter(f: A => Boolean): List[A] = foldLeft[List[A]](Nil)((xs, x) => if (f(x)) xs.append(x) else xs)

  /**
    * Ex 3.20
    * Write a function flatMap that works like map except that the function given will return a list
    * instead of a single result, and that list should be inserted into the final resulting list.
    * For instance, List(1,2,3).flatMap(i => List(i,i)) should result in  List(1,1,2,2,3,3).
    */
  def flatMap[B >: A](f: A => List[B]): List[B] = foldLeft[List[B]](Nil)((xs, x) => f(x).foldLeft(xs)(_.append(_)))

  /**
    * Ex 3.21
    * Use flatMap to implement filter
    */
  def filter2(f: A => Boolean): List[A] = flatMap(a => if (f(a)) List(a) else Nil)

  /**
    * Ex 3.23
    * Generalize `combine` so that it’s not specific to integers or addition. Name your generalized function zipWith.
    */
  def zipWith[B >: A, C](xs2: List[B])(f: (A, B) => C)(z: B): List[C] = {
    @tailrec
    def loop(xs: List[A], ys: List[B], acc: List[C]): List[C] = xs match {
      case Nil => acc
      case Cons(x, tx) =>
        ys match {
          case Cons(y, ty) => loop(tx, ty, acc.append(f(x, y)))
          case Nil => loop(tx, Nil, acc.append(f(x, z)))
        }
    }
    loop(this, xs2, Nil)
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

  /**
    * Ex 3.7
    * Can product, implemented using foldRight, immediately halt the recursion and return 0.0 if it encounters a 0.0?
    * Why or why not?
    * Consider how any short-circuiting might work if you call foldRight with a large list.
    * This is a deeper question that we’ll return to in chapter 5.
    *
    * Strictly speaking, our solution above is not using the same foldRight! We just implemented a different tail-recursive fold,
    * which defeats the point of this exercise(but is not bad programming).
    *
    * Trying to do this with the original implementation will lead to stack overflows. Keep drilling why - do it by expanding function
    * calls on paper...
    *
    */

  def product2(xs: List[Double]): Double = xs.foldRight(1.0D)(_ * _)
  def product2ShortCircuit(xs: List[Double]): (Double, Int) = xs.foldRightShortCircuit(1.0D)(_ * _)(_ == 0D)(0D)

  /**
    * Ex 3.11
    * Write sum, product, and a function to compute the length of a list using foldLeft.
    */

  def sumFoldLeft(xs: List[Int]): Int = xs.foldLeft(0)(_ + _)
  def productFoldLeft(xs: List[Double]): Double = xs.foldLeft(1D)(_ * _)
  def lengthFoldLeft(xs: List[_]): Int = xs.foldLeft(0)((l, _) => l + 1)

  /**
    * Ex 3.15 (Hard)
    *
    * Write a function that takes a list of lists and concatenates them into a single list.
    * Its runtime should be linear in the length of the input lists. Try to use functions we have already defined.
    */

  def concat[A](listOfLists: List[List[A]]): List[A] = {
    @tailrec
    def loop(list: List[A], lists: List[List[A]]): List[A] = lists match {
      case Nil => list
      case Cons(xs, listXs) => loop(xs.foldLeft(list)((ys, y) => ys.append(y)), listXs)
    }
    listOfLists match {
      case Cons(list, lists) => loop(list, lists)
      case _ => Nil
    }
  }

  /**
    * Ex 3.16
    * Write a function that transforms a list of integers by adding 1 to each element.
    * (Reminder: this should be a pure function that returns a new List!)
    */
  def increment(xs: List[Int]): List[Int] = xs.foldLeft[List[Int]](Nil)((ys, y) => ys.append(y + 1))

  /**
    * Ex 3.17
    * Write a function that turns each value in a List[Double] into a String.
    */
  def doublesAsStrings(xs: List[Double]): List[String] = xs.foldLeft[List[String]](Nil)((ys, y) => ys.append(y.toString))

  /**
    * Ex 3.22
    * Write a function that accepts two lists and constructs a new list by adding corresponding elements.
    * For example, List(1,2,3) and List(4,5,6) become List(5,7,9).
    */
  def combine(xs1: List[Int], xs2: List[Int]): List[Int] = {
    @tailrec
    def loop(xs: List[Int], ys: List[Int], acc: List[Int]): List[Int] = xs match {
      case Nil => acc
      case Cons(x, tx) =>
        ys match {
          case Cons(y, ty) => loop(tx, ty, acc.append(x + y))
          case Nil => loop(tx, Nil, acc.append(x))
        }
    }
    loop(xs1, xs2, Nil)
  }

  def combine1(xs1: List[Int], xs2: List[Int]): List[Int] = xs1.zipWith(xs2)(_ + _)(0)

}

