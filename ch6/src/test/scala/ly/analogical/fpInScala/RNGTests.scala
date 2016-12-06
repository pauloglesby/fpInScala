package ly.analogical.fpInScala

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class RNGTests extends BaseSpec with GeneratorDrivenPropertyChecks {

  import RNG._

  val genSimpleRNG = Gen.choose(Long.MinValue, Long.MaxValue).map(SimpleRNG)
  val genCount = Gen.choose(1, 100)

  describe("`nonNegativeInt` should") {

    it("produce integers only") {
      forAll(genSimpleRNG) { r =>
        val int = nonNegativeInt(r)._1
        int shouldBe an[Integer]
      }
    }

    it("produce non-negative values only") {
      forAll(genSimpleRNG) { r =>
        val int = nonNegativeInt(r)._1
        int should be >= 0
      }
    }

    it("effect deterministic sequence output via functional state") {
      forAll(genSimpleRNG) { r =>
        val rng = nonNegativeInt(r)._2
        val int1 = rng.nextInt._1
        val int2 = rng.nextInt._1
        int1 should equal(int2)
      }
    }

  }

  describe("`double` should") {

    it("produce double values between 0 and 1") {
      forAll(genSimpleRNG) { r =>
        val double = RNG.double(r)._1
        double should be >= 0D
        double should be < 1D
      }
    }

  }

  describe("`ints` should") {

    it("produce a list of integers of length `count`") {
      forAll(genCount, genSimpleRNG) { (n, r) =>
        ints(n)(r)._1.length should equal(n)
      }
    }

  }

  describe("`doubleViaMap` should") {

    it("produce double values between 0 and 1") {
      forAll(genSimpleRNG) { r =>
        val double = doubleViaMap(r)._1
        double should be >= 0D
        double should be < 1D
      }
    }

  }

  describe("`intsFromSequence` should") {

    it("produce a list of integers of length `count`") {
      forAll(genCount, genSimpleRNG) { (n, r) =>
        intsFromSequence(n)(r)._1.length should equal(n)
      }
    }

  }

}
