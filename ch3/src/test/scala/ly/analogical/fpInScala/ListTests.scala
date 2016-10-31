package ly.analogical.fpInScala

import scala.annotation.tailrec

import org.scalacheck.Gen
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class ListTests extends BaseSpec with GeneratorDrivenPropertyChecks {

  import List._

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
    *
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
    *
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
    *
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

    it("init method should return new list containing every element except the last") {
      val xs = List(1, 2, 3, 4)
      xs.init should equal(List(1, 2, 3))
    }

  }

  describe("product2 should") {

    val xs1 = List(1D, 2D, 3D, 4D)

    it("compute the product correctly for lists with non-zero elements") {
      product2(xs1) should equal(24D)
    }

    it("return 0 for lists with one or more zero elements") {
      val xs2 = Cons(0D, xs1)
      val xs3 = Cons(0D, xs2)
      product2(xs2) should equal(0D)
      product2(xs3) should equal(0D)
    }

    it("return 1 for empty lists") {
      product2(Nil) should equal(1D)
    }

  }

  describe("product2ShortCircuit should") {

    val xs1 = List(1D, 2D, 3D, 4D)

    it("compute the product correctly for lists with non-zero elements") {
      product2ShortCircuit(xs1) should equal((24D, 4))
    }

    it("short circuit and exit when reaching the first zero") {
      val withZero1 = List(1D, 2D, 3D, 0D, 4D)
      product2ShortCircuit(withZero1) should equal((0D, 4))
      val withZero2 = List(1D, 2D, 3D, 4D, 0D)
      product2ShortCircuit(withZero2) should equal((0D, 5))
      val withZero3 = List(0D, 2D, 3D, 4D, 0D)
      product2ShortCircuit(withZero3) should equal((0D, 1))
    }

  }

  describe("length with foldRight should") {

    it("return zero for an empty list") {
      Nil.length should equal(0)
    }

    it("return the correct length as the number of non-Nil elements") {
      forAll(genInts) { listSize =>
        val xs = genList(listSize)
        xs.length should equal(listSize)
      }
    }

  }

  describe("sumFoldLeft should") {

    it("return 0 for an empty list") {
      sumFoldLeft(Nil) should equal(0)
    }

    it("return the correct sum of elements for a non-empty list") {
      val xs1 = List(1)
      sumFoldLeft(xs1) should equal(1)
      val xs2 = List(1, 2, 3, 4)
      sumFoldLeft(xs2) should equal(10)
    }

  }

  describe("productFoldLeft should") {

    it("return 1 for an empty list") {
      productFoldLeft(Nil) should equal(1D)
    }

    it("return 0 for any list with a zero element") {
      val xs1 = List(0D)
      productFoldLeft(xs1) should equal(0D)
      val xs2 = List(1D, 2D, 0D, 4D)
      productFoldLeft(xs2) should equal(0D)
    }

    it("return the correct product of elements for a non-empty list") {
      val xs1 = List(2D)
      productFoldLeft(xs1) should equal(2D)
      val xs2 = List(1D, 2D, 3D, 4D)
      productFoldLeft(xs2) should equal(24D)
    }

  }

  describe("lengthFoldLeft should") {

    it("return 0 for an empty list") {
      lengthFoldLeft(Nil) should equal(0)
    }

    it("return the correct length as the number of non-Nil elements") {
      forAll(genInts) { listSize =>
        val xs = genList(listSize)
        lengthFoldLeft(xs) should equal(listSize)
      }
    }

  }

  describe("reverse2 should") {

    it("be the same as reverse") {
      forAll(genInts) { listSize =>
        val xs = genList(listSize)
        xs.reverse should equal(xs.reverse2)
      }
    }

  }

  describe("append should") {

    it("add an element to the end of a list") {
      val xs1 = List(1, 2, 3)
      val xs2 = List(1, 2, 3, 4)
      xs1.append(4) should equal(xs2)
    }

  }

  describe("concat should") {

    val xs1 = List(1, 2, 3)
    val xs2 = List(4, 5, 6)
    val all = List(1, 2, 3, 4, 5, 6)

    it("return the empty list if only one input of empty list is given") {
      concat(List(Nil)) should equal(Nil)
    }

    it("return the input non-empty list if only one input of non-empty list is given") {
      concat(List(xs1)) should equal(xs1)
    }

    it("concatenate a non-empty list with the empty list and return the non-empty list") {
      concat(List(xs1, Nil)) should equal(xs1)
    }

    it("concatenate an empty list with a non-empty list and return the non-empty list") {
      concat(List(Nil, xs1)) should equal(xs1)

    }
    it("concatenate two non-empty lists") {
      concat(List(xs1, xs2)) should equal(all)
    }

    it("concatenate two non-empty lists and one empty list") {
      concat(List(xs1, xs2, Nil)) should equal(all)
    }

    it("concatenate one non-empty list, one empty list, and one non-empty list") {
      concat(List(xs1, Nil, xs2)) should equal(all)
    }

    it("concatenate one empty list and two non-empty lists") {
      concat(List(Nil, xs1, xs2)) should equal(all)
    }

  }

  describe("increment should") {

    it("return the empty list if the input is the empty list") {
      increment(Nil) should equal(Nil)
    }

    it("return a new list of integers with each element incremented by 1") {
      val xs = List(1, 2, 3)
      increment(xs) should equal(List(2, 3, 4))
    }

  }

  describe("doublesAsStrings should") {

    it("return the empty list if the input is the empty list") {
      doublesAsStrings(Nil) should equal(Nil)
    }

    it("return a new list of integers with each element incremented by 1") {
      val xs = List(1D, 2D, 3D)
      doublesAsStrings(xs) should equal(List("1.0", "2.0", "3.0"))
    }

  }

  describe("map should") {

    it("be equivalent to increment when passed the function _ + 1") {
      val xs = List(1, 2, 3)
      xs.map(_ + 1) should equal(increment(xs))
    }

    it("be equivalent to doublesAsStrings when passed the function _.toString") {
      val xs = List(1D, 2D, 3D)
      xs.map(_.toString) should equal(doublesAsStrings(xs))
    }

  }

  describe("filter should") {

    val xs = List(1, 2, 3, 4)

    it("remove odd elements from a list of ints when passed _ % 2 == 0") {
      xs.filter(_ % 2 == 0) should equal(List(2, 4))
    }

    it("return an empty list if no elements satisfy the predicate") {
      xs.filter(_ % 5 == 0) should equal(Nil)
    }

  }

  describe("flatMap should") {

    it("return List(1, 1, 2, 2, 3, 3) when passed i => List(i, i) and called on List(1, 2, 3)") {
      List(1, 2, 3).flatMap(x => List(x, x)) should equal(List(1, 1, 2, 2, 3, 3))
    }

  }

}
