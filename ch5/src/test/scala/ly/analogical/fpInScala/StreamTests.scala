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

}
