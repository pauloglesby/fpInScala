package ly.analogical.fpinscala
package datatypes

sealed trait Option[+A] {

  /**
    * Ex 4.1
    * Implement `map`, `flatMap`, `getOrElse`, `orElse` and `filter`
    */
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(_) => this
    case None => ob
  }

  def filter(p: A => Boolean): Option[A] = this match {
    case Some(a) if p(a) => this
    case _ => None
  }

}
case class Some[A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  /**
    * Ex 4.2
    * Implement the `variance` function in terms of a `flatMap` on a `Seq[Double]`
    * If the `mean` of a sequence `xs` is `m`, the `variance` is the mean of `math.pow(x - m, 2)` for each element `x` of `xs`
    */
  def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)
  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(m => mean(xs.map(x => Math.pow(x - m, 2))))

  /**
    * Ex 4.3
    * Write a generic function, `map2`, that combines two `Option` values using a binary function. If either `Option` is
    * `None`, then `map2` returns `None`.
    */
  def map2[A, B, C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] = oa.flatMap(a => ob.map(b => f(a, b)))

  /**
    * Ex 4.4
    * Write a function sequence that combines a list of Options into one Option containing a list of all the Some values in the original list.
    * If the original list contains None even once, the result of the function should be None;
    * otherwise the result should be Some with a list of all the values.
    */
  def sequence[A](as: List[Option[A]]): Option[List[A]] = as match {
    case h :: t => h.flatMap(a => sequence(t).map(a :: _))
    case Nil => Some(Nil)
  }

  /**
    * Ex 4.5
    * Implement `traverse`, that combines map and sequence into a single pass over a list
    */
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = as match {
    case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    case Nil => Some(Nil)
  }

}