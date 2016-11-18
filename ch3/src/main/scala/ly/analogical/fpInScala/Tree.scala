package ly.analogical.fpInScala

import scala.annotation.tailrec

sealed trait Tree[+A] {
  /**
    * Ex 3.25
    * Write a function `size` that counts the number of nodes (leaves & branches) in a tree
    */
  def size: Int = {
    @tailrec
    def loop(ts: List[Tree[_]], acc: Int = 0): Int = ts match {
      case Nil => acc
      case Cons(tree, trees) => tree match {
        case Leaf(_) => loop(trees, acc + 1)
        case Branch(l, r) => loop(Cons(l, Cons(r, trees)), acc + 1)
      }
    }
    loop(List(this))
  }

}
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
