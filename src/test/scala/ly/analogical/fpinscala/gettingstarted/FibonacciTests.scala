package ly.analogical.fpinscala
package gettingstarted

import Fibonacci._
import common.BaseSpec

import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
/**
  * Created by Paul Oglesby on 12/03/2016.
  */
class FibonacciTests extends BaseSpec with PropertyChecks {

  describe("Fibonacci should") {

    it("return IllegalArgumentException for input less than zero") {
      val illegalFibArgs = Gen.choose(-200, -1)
      forAll (illegalFibArgs) { n =>
        val thrown = intercept[IllegalArgumentException] {
          fib(n)
        }
        assert(thrown.getMessage startsWith "requirement failed: Fibonacci accepts input >= 0 only. Received input ")
      }
    }

    it("return 1 for fib(0)") {
      fib(0) should equal(1)
    }

    it("return 1 for fib(1)") {
      fib(1) should equal(1)
    }

    it("return fib(n) = fib(n-1) + fib(n-2) for any n >= 2") {
      // no need to go to max theoretical limit, else we'll be waiting on tests a while...
      val fibArgs = Gen.choose(2, 200)
      forAll (fibArgs) { n =>
        fib(n) should equal(fib(n-1) + fib(n-2))
      }
    }

  }

}
