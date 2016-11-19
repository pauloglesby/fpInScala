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

  /**
    * Ex 3.27
    * Write a function `depth` that returns the maximum path length from the root of a tree to any leaf.
    */
  def depth: Int = {
    @tailrec
    def loop(ts: List[Tree[_]], d: Int = 0, m: Int = 0): Int = ts match {
      case Nil => m
      case Cons(tree, trees) => tree match {
        case Leaf(_) => loop(trees, d, m.max(d))
        case Branch(l, r) => loop(Cons(l, Cons(r, trees)), d + 1, m)
      }
    }
    loop(List(this))
  }

  /**
    * Ex 3.28
    * Write a function `map`, analogous to the `map` on `List`, that modifies each element of a `Tree` with a given function.
    */
  def map[B](f: A => B): Tree[B] = {
    def loop(t: Tree[A]): Tree[B] = t match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l, r) => Branch(loop(l), loop(r))
    }
    loop(this)
  }

}
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  /**
    * Ex 3.26
    * Write a function `maximum` that returns the maximum element in a `Tree[Int]`
    */
  def maximum(t: Tree[Int]) = {
    @tailrec
    def loop(ts: List[Tree[Int]], acc: Int = Integer.MIN_VALUE): Int = ts match {
      case Nil => acc
      case Cons(tree, trees) => tree match {
        case Leaf(v) => loop(trees, acc.max(v))
        case Branch(l, r) => loop(Cons(l, Cons(r, trees)), acc)
      }
    }
    loop(List(t))
  }

}
