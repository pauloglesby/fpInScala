package ly.analogical.fpInScala

import scala.annotation.tailrec

import org.scalacheck.Gen
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class ListTests extends BaseSpec with GeneratorDrivenPropertyChecks {

  private[ListTests] class ListMatcher(expected: List[_]) extends Matcher[List[_]] {
    def apply(actual: List[_]) = MatchResult(
      actual == expected,
      s"Failed: actual List $actual does not equal expected List $expected.",
      s"Failed: actual List $actual equals expected List $expected."
    )
  }

  def equal(xs: List[_]) = new ListMatcher(xs)

  @tailrec
  private[this] def genList[A](n: Int, xs: List[A] = Nil)(implicit genElem: Gen[A]): List[A] = {
    require(n >= 0, "n must be >= 0 to generate a list!")
    if (n == 0) xs
    else {
      val elem = genElem.sample.get
      genList(n - 1, Cons(elem, xs))
    }
  }

  implicit val genElem: Gen[Char] = Gen.alphaChar
  val genInts = Gen.choose(0, 100)

  describe("genList should") {

    it("return its `xs` argument if n = 0") {
      val xs = Cons(genElem.sample.get, Nil)
      genList(0) should equal(Nil)
      genList(0, xs) should equal(xs)
    }

    it("throw an IllegalArgumentException if n < 0") {
      forAll(Gen.choose(-100, -1)) { n =>
        val exception = intercept[IllegalArgumentException] {
          genList(n)
        }
        exception.getMessage should equal("requirement failed: n must be >= 0 to generate a list!")
      }
    }

  }

  describe("List") {

    it("tail method should return a new list containing every element of the original except the head") {
      forAll(genInts) { n =>
        val xs = genList(n)
        val newList = Cons(genElem.sample.get, xs)
        newList.tail should equal(xs)
      }
    }

    it("head method should return first Some(firstElement) if list non-empty, and None otherwise") {
      forAll(genInts) { n =>
        val xs = genList(n)
        val head = xs.head
        val check = xs match {
          case Cons(x, _) => head should equal(Some(x))
          case _ => head should equal(None)
        }
      }
    }

    it("setHead method should return a new list with a different head") {
      forAll(genInts) { n =>
        val xs = genList(n)
        val head = genElem.sample.get
        val newList = xs.setHead(head)
        newList.tail should equal(xs.tail)
        newList.head.foreach(_ should equal(head))
      }
    }

  }

}


