package ly.analogical
package fpInScala

import Fibonacci._

/**
  * Created by Paul Oglesby on 12/03/2016.
  */
class FibonacciTests extends BaseSpec {

  describe("Fibonacci should") {

    it("return 1 for fib(0)") {
      fib(0) should equal(1)
    }

  }

}
