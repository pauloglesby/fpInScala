package ly.analogical.fpInScala

class EitherTests extends BaseSpec {

  import Either._

  describe("`map` should") {

    it("return `Left(_)` when called on a `Left(_)`") {
      Left(1).map(_ => true) should equal(Left(1))
    }

    it("return `Right(f(x))` when called on a `Right(x)`") {
      Right(1).map(_ + 1) should equal(Right(2))
    }

  }

  describe("`flatMap` should") {

    it("return `Left(_)` when called on a `Left(_)`") {
      Left(1).flatMap(_ => Right(true)) should equal(Left(1))
    }

    it("return `f(x)` when called on a `Right(x)`") {
      Right(1).flatMap(r => Right(r.toString)) should equal(Right("1"))
      Right(1).flatMap(r => Left(r.toString)) should equal(Left("1"))
    }

  }

  describe("`orElse` should") {

    it("return itself when called on a `Right(_)`")  {
      Right(1).orElse(Right(2)) should equal(Right(1))
    }

    it("return the default when called on a `Left(_)`") {
      Left(1).orElse(Right(2)) should equal(Right(2))
    }

  }

  describe("`map2` should") {

    it("return `Left(a)` if `this` is a `Left(a)`") {
      val test: Either[Int, String] = Left(1)
      test.map2(Right(2))(_ + _) should equal(Left(1))
    }

    it("return `Left(b)` if the `Either` operand is a `Left(b)`") {
      val test: Either[Int, String] = Left(1)
      Right(1).map2(test)(_ + _) should equal(Left(1))
    }

    it("return `Right(f(a, b))` if both `Either`s are `Right`") {
      Right(1).map2(Right(2))(_ + _) should equal(Right(3))
    }

  }

  describe("`sequence` should") {

    it("return the first `Left` from a `List[Either]`") {
      sequence(List(Right(1), Left(2), Left(3), Right(4))) should equal(Left(2))
    }

    it("transform a List of `Right`s into a `Right(List(...))`") {
      sequence(List(Right(1), Right(2), Right(3), Right(4))) should equal(Right(List(1, 2, 3, 4)))
    }

  }

  describe("`traverse` should") {

    val f: Int => Either[String, String] = x => if (x % 2 == 1) Left(s"$x: bang!") else Right(x.toString)

    it("return the first `Left` from a `List[A]` with `f: A => Either[E, B]`") {
      traverse(List(4, 3, 2, 1))(f) should equal(Left("3: bang!"))
    }

    it("transform a List[A] of values mapping to `Right(b)` under `f` to a `Right[List[B]]`") {
      traverse(List(2, 4))(f) should equal(Right(List("2", "4")))
    }

  }

}
