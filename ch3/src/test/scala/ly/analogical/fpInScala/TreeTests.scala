package ly.analogical.fpInScala

import org.scalatest.prop.GeneratorDrivenPropertyChecks

class TreeTests extends BaseSpec with GeneratorDrivenPropertyChecks {

  val leaf = Leaf(1)
  val branch = Branch(leaf, leaf)

  describe("`size` should") {

    it("return 1 for a single-leaf tree") {
      leaf.size should equal(1)
    }

    it("return 3 for a single branch tree with 2 leaves") {
      branch.size should equal(3)
    }

    it("return 7 for a tree: 1 branch with 2 child branches each with 2 leaves") {
      Branch(branch, branch).size should equal(7)
    }

    it("return 5 for a tree: 1 branch with 1 leaf 1 child branch with 2 leaves") {
      Branch(branch, leaf).size should equal(5)
      Branch(leaf, branch).size should equal(5)
    }

  }

}
