package ly.analogical.fpInScala

class OptionTests extends BaseSpec {

  describe("`map` should") {

    it("return `None` when called on `None`") {
      None.map(_ => 1) should equal(None)
    }

    it("return a `Some` of the function called on the wrapped value when called on `Some(_)`") {
      Some(1).map(_ + 1) should equal(Some(2))
    }

  }

  describe("`flatMap` should") {

    it("return `None` when called on `None`") {
      None.flatMap(_ => Some(1)) should equal(None)
    }

    it("return the value of function called on the wrapped value when called on `Some(_)`") {
      Some(1).flatMap(Some(_)) should equal(Some(1))
    }

  }

  describe("`getOrElse` should") {

    it("return the wrapped value when called on `Some(_)`") {
      Some(1).getOrElse(2) should equal(1)
    }

    it("return the default when called on `None`") {
      None.getOrElse(2) should equal(2)
    }

  }

  describe("`orElse` should") {

    it("return itself when called on `Some(_)`") {
      Some(1).orElse(Some(2)) should equal(Some(1))
    }

    it("return the default option when called on `None`") {
      None.orElse(Some(2)) should equal(Some(2))
    }

  }

  describe("`filter` should") {

    it("return `None` when called on `None`") {
      None.filter(_ => true) should equal(None)
    }

    it("return `None` when called on `Some(x)` if `p(x) == false`") {
      Some(1).filter(_ == 2) should equal(None)
    }

    it("return `Some(x)` when called on `Some(x)` if `p(x) == true`") {
      Some(1).filter(_ == 1) should equal(Some(1))
    }

  }

}
