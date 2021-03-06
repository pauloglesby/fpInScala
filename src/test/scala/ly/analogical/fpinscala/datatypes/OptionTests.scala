package ly.analogical.fpinscala
package datatypes

import common.BaseSpec

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class OptionTests extends BaseSpec with GeneratorDrivenPropertyChecks {

  import Option._
  val genInts = Gen.choose(2, 100)

  describe("`map` should") {

    it("return `None` when called on `None`") {
      datatypes.None.map(_ => 1) should equal(datatypes.None)
    }

    it("return a `Some` of the function called on the wrapped value when called on `Some(_)`") {
      datatypes.Some(1).map(_ + 1) should equal(datatypes.Some(2))
    }

  }

  describe("`flatMap` should") {

    it("return `None` when called on `None`") {
      datatypes.None.flatMap(_ => datatypes.Some(1)) should equal(datatypes.None)
    }

    it("return the value of function called on the wrapped value when called on `Some(_)`") {
      datatypes.Some(1).flatMap(datatypes.Some(_)) should equal(datatypes.Some(1))
    }

  }

  describe("`getOrElse` should") {

    it("return the wrapped value when called on `Some(_)`") {
      datatypes.Some(1).getOrElse(2) should equal(1)
    }

    it("return the default when called on `None`") {
      datatypes.None.getOrElse(2) should equal(2)
    }

  }

  describe("`orElse` should") {

    it("return itself when called on `Some(_)`") {
      datatypes.Some(1).orElse(datatypes.Some(2)) should equal(datatypes.Some(1))
    }

    it("return the default option when called on `None`") {
      datatypes.None.orElse(datatypes.Some(2)) should equal(datatypes.Some(2))
    }

  }

  describe("`filter` should") {

    it("return `None` when called on `None`") {
      datatypes.None.filter(_ => true) should equal(datatypes.None)
    }

    it("return `None` when called on `Some(x)` if `p(x) == false`") {
      datatypes.Some(1).filter(_ == 2) should equal(datatypes.None)
    }

    it("return `Some(x)` when called on `Some(x)` if `p(x) == true`") {
      datatypes.Some(1).filter(_ == 1) should equal(datatypes.Some(1))
    }

  }

  describe("`mean` should") {

    it("return `None` on an empty sequence") {
      mean(Seq.empty[Double]) should equal(datatypes.None)
    }

    it("return the arithmetic mean on a non-empty sequence") {
      forAll(genInts) { n =>
        // use mathematical facts about sums of consecutive sequences of integers from 1
        val expected = (n.toDouble + 1) / 2
        mean((1 to n).map(_.toDouble)) should equal(datatypes.Some(expected))
      }
    }

  }

  describe("`variance` should") {

    it("return `None` on an empty sequence") {
      variance(Seq.empty[Double]) should equal(datatypes.None)
    }

    it("return the variance on a non-empty sequence") {
      forAll(genInts) { n =>
        // use fact that variance of 3 consecutive integers should be 2/3
        val expected = 2D / 3
        variance((n to (n + 2)).map(_.toDouble)) should equal(datatypes.Some(expected))
      }
    }

  }

  describe("`map2` should") {

    it("return `None` if the first input is `None`") {
      map2[Int, Int, Int](datatypes.None, datatypes.Some(1))(_ + _) should equal(datatypes.None)
    }

    it("return `None` if the second input is `None`") {
      map2[Int, Int, Int](datatypes.Some(1), datatypes.None)(_ + _) should equal(datatypes.None)
    }

    it("return `None` if the both inputs are `None`") {
      map2[Int, Int, Int](datatypes.None, datatypes.None)(_ + _) should equal(datatypes.None)
    }

    it("return a `Some` of the mapped input values if both inputs are `Some(_)`") {
      map2[Int, Int, Int](datatypes.Some(1), datatypes.Some(1))(_ + _) should equal(datatypes.Some(2))
    }

  }

  describe("`sequence` should") {

    it("return `None` if any input element is `None`") {
      sequence(List(datatypes.Some(1), datatypes.None)) should equal(datatypes.None)
      sequence(List(datatypes.None, datatypes.Some(1))) should equal(datatypes.None)
      sequence(List(datatypes.None, datatypes.None)) should equal(datatypes.None)
    }

    it("return `Some(List(_))` if all the input elements are `Some(_)`") {
      sequence(List(datatypes.Some(1), datatypes.Some(2), datatypes.Some(3))) should equal(datatypes.Some(List(1, 2, 3)))
    }

  }

  describe("`traverse` should") {

    val f: Int => datatypes.Option[Int] = x => if (x % 2 == 0) datatypes.Some(x) else datatypes.None

    it("return `None` if any element maps to `None` under `f`") {
      traverse(List(1))(f) should equal(datatypes.None)
      traverse(List(3, 1))(f) should equal(datatypes.None)
      traverse(List(1, 2))(f) should equal(datatypes.None)
      traverse(List(2, 1))(f) should equal(None)
    }

    it("return `Some(List(_))` if all elements map to `Some(_)` under `f`") {
      traverse(List(2, 4))(f) should equal(Some(List(2, 4)))
    }

  }

}
