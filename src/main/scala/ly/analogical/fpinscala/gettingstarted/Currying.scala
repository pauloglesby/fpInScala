package ly.analogical.fpinscala
package gettingstarted

/**
  * Created by Paul Oglesby on 11/03/2016.
  */
object Currying {

  /**
    * Ex 2.3: implement a `curry` function that takes a function with args (arg, f) and partially applies f to
    * return another function
    */

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = { a: A =>
    b: B => f(a, b)
  }

  /**
    * Ex 2.4 implement `uncurry`, which reverses the transformation of `curry`
    */

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = { (a: A, b: B) =>
    f(a)(b)
  }

}
