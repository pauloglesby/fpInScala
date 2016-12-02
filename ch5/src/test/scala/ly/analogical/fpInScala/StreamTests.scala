package ly.analogical.fpInScala

class StreamTests extends BaseSpec {

  import Stream._

  val p: Int => Boolean = _ % 2 == 0

  describe("`toList` should") {

    it("convert an `Empty` to `Nil`") {
      Empty.toList should equal(Nil)
    }

    it("convert a `Stream[Int]` to a `List[Int]`") {
      Stream(1, 2, 3).toList should equal(List(1, 2, 3))
    }

  }

  describe("`take` should") {

    it("return an `Empty` when called on `Empty`") {
      Empty.take(1) should equal(Empty)
    }

    it("return the first n elements of the Stream as a Stream") {
      // NB need to evaluate the stream to test it, so use `toList`!
      // otherwise we just end up looking at uncalled anonymous functions
      Stream(1, 2, 3, 4).take(3).toList should equal(List(1, 2, 3))
    }

  }

  describe("`drop` should") {

    it("return an `Empty` when called on `Empty`") {
      Empty.drop(1) should equal(Empty)
    }


    it("return the Stream succeeding the first n elements as a Stream") {
      val test = Stream(1, 2, 3, 4).drop(2)
      test.toList should equal(List(3, 4))
    }

  }


  describe("`takeWhile` should") {

    it("return an `Empty` when called on `Empty`") {
      Empty.takeWhile(p) should equal(Empty)
    }

    it("return `Empty` if none of the elements satisfy the predicate") {
      Stream(1, 3, 5).takeWhile(p) should equal(Empty)
    }

    it("return only the elements that satisfy the predicate") {
      Stream(1, 2, 3, 4).takeWhile(p).toList should equal(List(2, 4))
    }

  }

  describe("`forAll` should") {

    it("return `true` for an empty stream") {
      Empty.forAll(p) should equal(true)
    }

    it("return true only if all of the elements of the stream satisfy the predicate") {
      Stream(2, 4, 6, 8).forAll(p) should equal(true)
    }

    it("return false and stop traversing as soon as one element negates the predicate") {
      var ix = 0
      def check(x: Int): Boolean = { ix += 1; p(x) }
      Stream(2, 4, 5, 8, 9).forAll(check) should equal(false)
      ix should equal(3)
    }

  }

  describe("`takeWhileFromFoldRight` should") {

    it("return `Empty` when called on `Empty`") {
      Empty.takeWhileFromFoldRight(p) should equal(Empty)
    }

    it("return `Empty` if none of the elements satisfy the predicate") {
      Stream(1, 3, 5).takeWhileFromFoldRight(p) should equal(Empty)
    }

    it("return only the elements that satisfy the predicate") {
      Stream(1, 2, 3, 4).takeWhileFromFoldRight(p).toList should equal(List(2, 4))
    }

  }

  describe("`headOptionFromFoldRight` should") {

    it("return `None` when called on `Empty`") {
      Empty.headOptionFromFoldRight should equal(None)
    }

    it("return `Some(h)` when called on a non-empty stream") {
      Stream(1, 2).headOptionFromFoldRight should equal(Some(1))
    }

  }

  describe("`map` should") {

    val f: Int => String = _.toString

    it("return `Empty` when called on `Empty`") {
      Empty.map(f) should equal(Empty)
    }

    it("return a stream of mapped values") {
      Stream(1, 2).map(f).toList should equal(List("1", "2"))
    }

  }

  describe("`filter` should") {

    it("return `Empty` when called on `Empty`") {
      Empty.filter(p) should equal(Empty)
    }

    it("return a stream of values satisfying the predicate") {
      Stream(1, 2, 3, 4).filter(p).toList should equal(List(2, 4))
    }

  }

  describe("`append` should") {

    it("append a stream to the end of a stream") {
      Empty.append(Stream(1)).toList should equal(List(1))
      Stream(1, 2).append(Stream(3, 4)).toList should equal(List(1, 2, 3, 4))
    }

  }

  describe("`flatMap` should") {

    def streamFrom(x: Int): Stream[Int] = Stream(x, x + 1)

    it("return `Empty` when called on `Empty`") {
      Empty.flatMap(streamFrom) should equal(Empty)
    }

    it("map elements to streams and output a composite stream") {
      Stream(1, 3, 5).flatMap(streamFrom).toList should equal(List(1, 2, 3, 4, 5, 6))
    }

  }

  describe("`constant` should") {

    it("produce an infinite stream of the input value") {
      constant("hi!").take(3).toList should equal(List("hi!", "hi!", "hi!"))
    }

  }

  describe("`from` should") {

    it("produce an infinite stream of consecutive integers from n") {
      from(5).take(3).toList should equal(List(5, 6, 7))
    }

  }

  describe("`fibs` should") {

    it("produce an infinite stream of fibonacci numbers") {
      fibs.take(10).toList should equal(List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
    }

  }

}
