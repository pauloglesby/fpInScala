package ly.analogical.fpInScala

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class TreeTests extends BaseSpec with GeneratorDrivenPropertyChecks {

  import Tree._

  val leaf = Leaf(1)
  val branch = Branch(leaf, leaf)

  def genLeaf = Gen.choose(Int.MinValue, Int.MaxValue).map(Leaf(_))

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

  describe("`maximum` should") {

    it("return the element value for a single leaf tree") {
      forAll(genLeaf) { l =>
        maximum(l) should equal(l.value)
      }
    }

    it("return the largest element in a branch with 2 leaves") {
      maximum(Branch(Leaf(-2), Leaf(-1))) should equal(-1)
      maximum(Branch(Leaf(-1), Leaf(-2))) should equal(-1)
    }

    it("return the largest element in a multi branch tree") {
      val tree = Branch(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Branch(Leaf(4), Leaf(5))), Branch(Leaf(6), Leaf(7)))
      maximum(tree) should equal(7)
    }

  }

  describe("`depth` should") {

    it("return 0 for a single leaf tree") {
      leaf.depth should equal(0)
    }

    it("return 1 for a single branch tree with 2 leaves") {
      branch.depth should equal(1)
    }

    it("return 2 for a tree with two leaves at depth 2, and one leaf at depth 1") {
      Branch(branch, leaf).depth should equal(2)
      Branch(leaf, branch).depth should equal(2)
    }

  }

  describe("`map` should") {

    it("transform single leaf element with provided function") {
      leaf.map(_.toString) should equal(Leaf("1"))
    }

    it("preserve outer structure of tree") {
      val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
      tree.map(_.toString) should equal(Branch(Branch(Leaf("1"), Leaf("2")), Branch(Leaf("3"), Leaf("4"))))
    }

  }

}
