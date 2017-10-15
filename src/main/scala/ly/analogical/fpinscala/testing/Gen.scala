package ly.analogical.fpinscala
package testing

import Prop._
import state.{RNG, SimpleRNG, State}

trait Prop {
  def check: Either[(String, SuccessCount), SuccessCount]

  //def &&(p: Prop): Prop = if (check) p else this
}

object Prop {
  type SuccessCount = Int
  type FailedCase = String
}


case class Gen[A](sample: State[RNG, A])

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val transition: RNG => (Int, RNG) = rng => {
      val (value, next) = rng.nextInt
      if (start <= value && value < stopExclusive) (value, next) else transition(next)
    }
    val state = State(transition)
    Gen(state)
  }

}