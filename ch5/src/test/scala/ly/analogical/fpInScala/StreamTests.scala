package ly.analogical.fpInScala

class StreamTests extends BaseSpec {

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

}
