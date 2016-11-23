package ly.analogical.fpInScala

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class OptionTests extends BaseSpec with GeneratorDrivenPropertyChecks {

  import Option._
  val genInts = Gen.choose(2, 100)

  describe("`map` should") {

    it("return `None` when called on `None`") {
      None.map(_ => 1) should equal(None)
    }

    it("return a `Some` of the function called on the wrapped value when called on `Some(_)`") {
      Some(1).map(_ + 1) should equal(Some(2))
    }

  }

  describe("`flatMap` should") {

    it("return `None` when called on `None`") {
      None.flatMap(_ => Some(1)) should equal(None)
    }

    it("return the value of function called on the wrapped value when called on `Some(_)`") {
      Some(1).flatMap(Some(_)) should equal(Some(1))
    }

  }

  describe("`getOrElse` should") {

    it("return the wrapped value when called on `Some(_)`") {
      Some(1).getOrElse(2) should equal(1)
    }

    it("return the default when called on `None`") {
      None.getOrElse(2) should equal(2)
    }

  }

  describe("`orElse` should") {

    it("return itself when called on `Some(_)`") {
      Some(1).orElse(Some(2)) should equal(Some(1))
    }

    it("return the default option when called on `None`") {
      None.orElse(Some(2)) should equal(Some(2))
    }

  }

  describe("`filter` should") {

    it("return `None` when called on `None`") {
      None.filter(_ => true) should equal(None)
    }

    it("return `None` when called on `Some(x)` if `p(x) == false`") {
      Some(1).filter(_ == 2) should equal(None)
    }

    it("return `Some(x)` when called on `Some(x)` if `p(x) == true`") {
      Some(1).filter(_ == 1) should equal(Some(1))
    }

  }

  describe("`mean` should") {

    it("return `None` on an empty sequence") {
      mean(Seq.empty[Double]) should equal(None)
    }

    it("return the arithmetic mean on a non-empty sequence") {
      forAll(genInts) { n =>
        // use mathematical facts about sums of consecutive sequences of integers from 1
        val expected = (n.toDouble + 1) / 2
        mean((1 to n).map(_.toDouble)) should equal(Some(expected))
      }
    }

  }

  describe("`variance` should") {

    it("return `None` on an empty sequence") {
      variance(Seq.empty[Double]) should equal(None)
    }

    it("return the variance on a non-empty sequence") {
      forAll(genInts) { n =>
        // use fact that variance of 3 consecutive integers should be 2/3
        val expected = 2D / 3
        variance((n to (n + 2)).map(_.toDouble)) should equal(Some(expected))
      }
    }

  }

}
