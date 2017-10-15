package ly.analogical.fpinscala
package datatypes

sealed trait Either[+E, +A] {
  /**
    * Ex 4.6
    * Implement versions of `map`, `flatMap`, `orElse`, and `map2` on `Either` that operate on the `Right` value.
    */

  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => Right(a)
    case _ => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
    ra <- this
    rb <- b
  } yield f(ra, rb)

}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  /**
    * Ex 4.7
    * Implement sequence and traverse for Either. These should return the first error that’s encountered, if there is one.
    */
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil => Right(Nil)
    case Left(e) :: _  => Left(e)
    case Right(a) :: t => Right(a).map2(sequence(t))(_ :: _)
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case a :: t => f(a).map2(traverse(t)(f))(_ :: _)
  }

}