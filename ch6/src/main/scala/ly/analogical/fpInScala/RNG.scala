package ly.analogical.fpInScala

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

}

object RNG {

  /**
    * Ex 6.1
    * Write a function that uses RNG.nextInt to generate a random integer between 0 and Int.maxValue (inclusive).
    * Make sure to handle the corner case when nextInt returns Int.MinValue, which doesn’t have a non-negative counterpart.
    */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (int, nextRng) = rng.nextInt
    (if (int < 0) -(int + 1) else int, nextRng)
  }

  /**
    * Ex 6.2
    * Write a function to generate a Double between 0 and 1, not including 1.
    */
  def double(rng: RNG): (Double, RNG) = {
    val (int, nextRng) = nonNegativeInt(rng)
    (int / (Int.MaxValue.toDouble + 1), nextRng)
  }

  /**
    * Ex 6.3
    * Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a (Double, Double, Double) 3-tuple.
    * You should be able to reuse the functions you’ve already written.
    */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  /**
    * Ex 6.4
    * Write a function to generate a list of random integers.
    */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = (1 to count).foldRight((List.empty[Int], rng)) { case (_, (ints, r)) =>
    val (i, rNext) = r.nextInt
    (i :: ints, rNext)
  }


  type State[S, +A] = S => (A, S)
  type Rand[+A] = State[RNG, A]

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  /**
    * Ex 6.5
    * Use map to reimplement double in a more elegant way.
    */
  def doubleViaMap: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  /**
    * Ex 6.6
    * Write the implementation of map2 based on the following signature.
    * This function takes two actions, ra and rb, and a function f for combining their results, and returns a new action that combines them:
    */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, r1) = ra(rng)
    val (b, r2) = rb(r1)
    (f(a, b), r2)
  }

  /**
    * Ex 6.7
    * Hard: If you can combine two RNG transitions, you should be able to combine a whole list of them.
    * Implement sequence for combining a List of transitions into a single transition.
    * Use it to reimplement the ints function you wrote before. For the latter, you can use the
    * standard library function List.fill(n)(x) to make a list with x repeated n times.
    */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(List[A]()))((f, ras) => map2(f, ras)(_ :: _))

  def intsFromSequence(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  /**
    * Ex 6.8
    * Implement flatMap, and then use it to implement nonNegativeLessThan
    */
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  }

  /**
    * Ex 6.9
    * Reimplement map and map2 in terms of flatMap.
    * The fact that this is possible is what we mean when we say that flatMap is "more powerful" than map and map2.
    */
  def mapFromFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2FromFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

}

