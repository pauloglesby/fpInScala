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

  /**
    * Helper method to check lengths without creating premature skyhooks in the List trait!
    * May result in duplication though...
    * @param xs List
    * @tparam A
    * @return length of list, Int
    */
  private[this] def length[A](xs: List[A]): Int = {
    @tailrec
    def loop(ys: List[A], l: Int): Int = ys match {
      case Nil => l
      case _ => loop(ys.tail, l + 1)
    }
    loop(xs, 0)
  }

  /**
    * Helper method to assert that each element in a List satisfies an assertion
    * @param xs
    * @param assertion
    * @tparam A
    */
  @tailrec
  private[this] def assertElems[A](xs: List[A])(assertion: A => Unit): Unit = xs match {
    case Cons(head, Nil) =>
      assertion(head)
    case Cons(head, tail) =>
      assertion(head)
      assertElems(tail)(assertion)
    case _ => Unit
  }

  /**
    * Helper method to assert that each element in a List satisfies an assertion
    * @param xs
    * @param assertion
    * @tparam A
    */
  @tailrec
  private[this] def assertElemsNonEmpty[A](xs: List[A])(assertion: A => Unit): Unit = xs match {
    case Nil => assert(false, "Input list is empty")
    case Cons(head, tail) =>
      assertion(head)
      tail match {
        case Cons(_, _) => assertElemsNonEmpty(tail)(assertion)
        case _ => Unit
      }
    case _ => Unit
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

  describe("length should") {
    it("return size of list as number of elements") {
      forAll(genInts) { n =>
        val xs = genList(n)
        length(xs) should equal(n)
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

    it("drop method should return new list with first n elements dropped") {
      forAll(genInts.suchThat(_ >= 1)) { listSize =>
        forAll(Gen.choose(0, listSize)) { dropCount =>
          val xs = genList(listSize)
          val newList = xs.drop(dropCount)
          length(newList) should equal(length(xs) - dropCount)
        }
      }
    }

    it("dropWhile method should return new list containing elements where predicate is not matched") {
      forAll(genInts) { listSize =>
        val xs = genList(listSize)(genInts)
        val predicate: Int => Boolean = _ % 2 == 0
        val newList = xs.dropWhile(predicate)
        assertElems(newList)(predicate(_) should equal(false))
      }
    }

  }

}


