package ly.analogical.fpInScala

/**
  * Created by Paul Oglesby on 11/03/2016.
  */
object Compose {

  /**
    * Ex 2.5: Implement the HOF that composes two functions.
    */

  def compose[A, B, C](f: B => C, g: A => B): A => C = { a: A =>
    f(g(a))
  }

}
