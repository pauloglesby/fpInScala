package ly.analogical.fpInScala

import ly.analogical.fpInScala.Prop.SuccessCount

trait Prop {
  def check: Either[(String, SuccessCount), SuccessCount]

  //def &&(p: Prop): Prop = if (check) p else this
}

object Prop {
  type SuccessCount = Int
  type FailedCase = String
}


case class Gen[A](sample: State[RNG, A])