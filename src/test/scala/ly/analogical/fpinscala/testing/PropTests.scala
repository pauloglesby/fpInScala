package ly.analogical.fpinscala
package testing

import common.BaseWordSpec

import scala.math.Ordering.Implicits._

class PropTests extends BaseWordSpec {

  import Prop._
  import Gen._

  private val smallInt = Gen.choose(-10, 10)

  "Properties" should {

    "list.max should be greater than or equal to every other list element" in {
      val maxProp = forAll(listOf1(smallInt)) { ns =>
        val max = ns.max
        !ns.exists(_ > max)
      }
      Prop.run(maxProp) should equal(Passed)
    }

    "each element of a sorted list should be greater than or equal to its predecessor" in {
      val sortedProp = forAll(listOf1(smallInt)) { ns =>
        val sorted = ns.sorted
        def isSorted(xs: List[Int], prev: Int = Int.MinValue): Boolean = xs match {
          case h :: tail if h >= prev => isSorted(tail, h)
          case h :: tail if h < prev => false
          case _ => true
        }
        isSorted(sorted)
      }
      Prop.run(sortedProp) should equal(Passed)
    }

  }

}
