package ly.analogical.fpInScala

import scala.annotation.tailrec

/**
  * Created by Paul Oglesby on 11/03/2016.
  */
object Fibonacci {

  /**
    * Ex 2.1: Write a recursive function to get the nth Fibonacci number
    */

  def fib(n: Int): Int = {
    require(n >= 0, s"Fibonacci accepts input >= 0 only. Received input $n.")
    @tailrec
    def loop(n: Int, a: Int, b: Int): Int = {
      if (n == 0) b
      else loop(n - 1, b, a + b)
    }
    loop(n, 0, 1)
  }

}
