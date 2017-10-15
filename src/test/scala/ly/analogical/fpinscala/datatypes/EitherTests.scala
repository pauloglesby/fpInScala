package ly.analogical.fpinscala
package datatypes

import common.BaseSpec


class EitherTests extends BaseSpec {

  import Either._

  describe("`map` should") {

    it("return `Left(_)` when called on a `Left(_)`") {
      datatypes.Left(1).map(_ => true) should equal(datatypes.Left(1))
    }

    it("return `Right(f(x))` when called on a `Right(x)`") {
      datatypes.Right(1).map(_ + 1) should equal(datatypes.Right(2))
    }

  }

  describe("`flatMap` should") {

    it("return `Left(_)` when called on a `Left(_)`") {
      datatypes.Left(1).flatMap(_ => datatypes.Right(true)) should equal(datatypes.Left(1))
    }

    it("return `f(x)` when called on a `Right(x)`") {
      datatypes.Right(1).flatMap(r => datatypes.Right(r.toString)) should equal(datatypes.Right("1"))
      datatypes.Right(1).flatMap(r => datatypes.Left(r.toString)) should equal(datatypes.Left("1"))
    }

  }

  describe("`orElse` should") {

    it("return itself when called on a `Right(_)`")  {
      datatypes.Right(1).orElse(datatypes.Right(2)) should equal(datatypes.Right(1))
    }

    it("return the default when called on a `Left(_)`") {
      datatypes.Left(1).orElse(datatypes.Right(2)) should equal(datatypes.Right(2))
    }

  }

  describe("`map2` should") {

    it("return `Left(a)` if `this` is a `Left(a)`") {
      val test: datatypes.Either[Int, String] = datatypes.Left(1)
      test.map2(datatypes.Right(2))(_ + _) should equal(datatypes.Left(1))
    }

    it("return `Left(b)` if the `Either` operand is a `Left(b)`") {
      val test: datatypes.Either[Int, String] = datatypes.Left(1)
      datatypes.Right(1).map2(test)(_ + _) should equal(datatypes.Left(1))
    }

    it("return `Right(f(a, b))` if both `Either`s are `Right`") {
      datatypes.Right(1).map2(datatypes.Right(2))(_ + _) should equal(datatypes.Right(3))
    }

  }

  describe("`sequence` should") {

    it("return the first `Left` from a `List[Either]`") {
      sequence(List(datatypes.Right(1), datatypes.Left(2), datatypes.Left(3), datatypes.Right(4))) should equal(datatypes.Left(2))
    }

    it("transform a List of `Right`s into a `Right(List(...))`") {
      sequence(List(datatypes.Right(1), datatypes.Right(2), datatypes.Right(3), datatypes.Right(4))) should equal(datatypes.Right(List(1, 2, 3, 4)))
    }

  }

  describe("`traverse` should") {

    val f: Int => datatypes.Either[String, String] = x => if (x % 2 == 1) datatypes.Left(s"$x: bang!") else datatypes.Right(x.toString)

    it("return the first `Left` from a `List[A]` with `f: A => Either[E, B]`") {
      traverse(List(4, 3, 2, 1))(f) should equal(Left("3: bang!"))
    }

    it("transform a List[A] of values mapping to `Right(b)` under `f` to a `Right[List[B]]`") {
      traverse(List(2, 4))(f) should equal(Right(List("2", "4")))
    }

  }

}
