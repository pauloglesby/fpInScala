package ly.analogical.fpInScala

case class State[S, +A](run: S => (A, S)) {

  import State._

  /**
    * Ex 6.10
    * Generalize the functions unit, map, map2, flatMap, and sequence.
    * Add them as methods on the State case class where possible.
    * Otherwise you should put them in a State companion object.
    */

  def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s1 => {
    val (a, s2) = run(s1)
    f(a).run(s2)
  })

}

object State {

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] =
    ss.foldRight(unit[S, List[A]](List.empty[A]))((s, acc) => s.map2(acc)(_ :: _))

}

