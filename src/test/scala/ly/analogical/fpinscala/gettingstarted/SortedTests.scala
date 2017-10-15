package ly.analogical.fpinscala
package gettingstarted

import Sorted._
import common.BaseSpec

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks

/**
  * Created by Paul Oglesby on 03/04/2016.
  */
class SortedTests extends BaseSpec with GeneratorDrivenPropertyChecks {

  describe("isSorted should") {

    def stringOrderAsc(a: String, b: String): Boolean = a <= b
    def stringOrderDesc(a: String, b: String): Boolean = a >= b
    def intOrderAsc(a: Int, b: Int) = (a <= b)
    def intOrderDesc(a: Int, b: Int) = (a >= b)

    val genIntArray = Gen.containerOf[Array, Int](
      Gen.choose(Int.MinValue, Int.MaxValue)
    )

    val genSortedAscIntArray = genIntArray.filter(_.toSet.size > 1).map(_.sorted)
    val genSortedDescIntArray = genIntArray.filter(_.toSet.size > 1).map(_.sorted(Ordering[Int].reverse))

    it("return true for an empty array of String with ascending string order comparator") {
      isSorted[String](Array.empty[String], stringOrderAsc) should equal(true)
    }

    it("return true for an array of single String with ascending string order comparator") {
      isSorted[String](Array("a"), stringOrderAsc) should equal(true)
    }

    it("return true for an empty array of String with descending string order comparator") {
      isSorted[String](Array.empty[String], stringOrderDesc) should equal(true)
    }

    it("return true for an array of single String with descending string order comparator") {
      isSorted[String](Array("a"), stringOrderDesc) should equal(true)
    }

    it("return true for an empty array of Int with ascending string order comparator") {
      isSorted[Int](Array.empty[Int], intOrderAsc) should equal(true)
    }

    it("return true for an array of single string with ascending string order comparator") {
      isSorted[Int](Array(0), intOrderAsc) should equal(true)
    }

    it("return true for an empty array of strings with descending string order comparator") {
      isSorted[Int](Array.empty[Int], intOrderDesc) should equal(true)
    }

    it("return true for an array of single string with descending string order comparator") {
      isSorted[Int](Array(0), intOrderDesc) should equal(true)
    }

    it("return true for integer arrays with multiple entries that are sorted and compared ascendingly") {
      forAll(genSortedAscIntArray) { xs: Array[Int] =>
        isSorted(xs, intOrderAsc) should equal(true)
      }
    }

    it("return true for integer arrays with multiple entries that are sorted and compared descendingly") {
      forAll(genSortedDescIntArray) { xs: Array[Int] =>
        isSorted(xs, intOrderDesc) should equal(true)
      }
    }

    it("return false for integer arrays with multiple entries that are sorted ascendingly and compared descendingly") {
      forAll(genSortedAscIntArray) { xs: Array[Int] =>
        isSorted(xs, intOrderDesc) should equal(false)
      }
    }

    it("return false for integer arrays with multiple entries that are sorted descendingly and compared ascendingly") {
      forAll(genSortedDescIntArray) { xs: Array[Int] =>
        isSorted(xs, intOrderAsc) should equal(false)
      }
    }

    it("return false for unordered integer arrays") {
      forAll(genSortedAscIntArray, genSortedDescIntArray) { (asc, desc) =>
        isSorted(asc ++ desc, intOrderAsc) should equal(false)
        isSorted(asc ++ desc, intOrderDesc) should equal(false)
      }
    }

  }

}
