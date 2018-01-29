package ly.analogical.fpinscala
package testing

import Prop._
import state.{RNG, SimpleRNG, State}
import laziness.Stream

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  def &&(p: Prop): Prop = Prop(
    (max, n, rng) => {
      val result = run(max, n, rng)
      val pResult = p.run(max, n, rng)
      (result, pResult) match {
        case (Passed, Passed) => Passed
        case (f@Falsified(_, _), _) => f
        case (_, f@Falsified(_, _)) => f
      }
    }
  )

  def ||(p: Prop): Prop = Prop(
    (max, n, rng) => {
      val result = run(max, n, rng)
      val pResult = p.run(max, n, rng)
      (result, pResult) match {
        case (Passed, _) => Passed
        case (_, Passed) => Passed
        case (f@Falsified(_, _), Falsified(_, _)) => f
      }
    }
  )

}

object Prop {
  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int
  type MaxSize = Int

  sealed trait Result extends Product with Serializable {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    override def isFalsified: Boolean = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    override def isFalsified: Boolean = true
  }

  def run(p: Prop,
          maxSize: MaxSize = 100,
          testCases: TestCases = 100,
          rng: RNG = SimpleRNG(System.currentTimeMillis())
         ): Result = {
    p.run(maxSize, testCases, rng) match {
      case r: Falsified =>
        println(s"! Falsified after ${r.successes} passed tests:\n ${r.failure}")
        r
      case r: Passed.type =>
        println(s"+ OK, passed $testCases tests.")
        r
    }
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (_, n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  implicit def unsized[A](g: Gen[A]): SGen[A] = SGen(_ => g)

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop(
    (max, n, rng) => {
      val casesPerSize: TestCases = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
    }
  )

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(r => Some(g.sample.run(r)))

  def buildMsg[A](a: A, exception: Exception): String =
    s"test case: $a\n" +
    s"generated an exception: ${exception.getMessage}\n" +
    s"stack trace: \n ${exception.getStackTrace.mkString("\n")}"

}

case class Gen[A](sample: State[RNG, A]) {

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(f(_).sample))

  def listOfN(n: Int): Gen[List[A]] =
    Gen.listOfN(n, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(listOfN)

  def unsized: SGen[A] = SGen(_ => this)

}

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))


  def char: Gen[Char] = {
    val ASCII_PRINTABLE_START = 33
    val ASCII_PRINTABLE_END = 176
    Gen(choose(ASCII_PRINTABLE_START, ASCII_PRINTABLE_END).sample.map(_.toChar))
  }

  def unit[A](a: => A): Gen[A] = {
    val transition: RNG => (A, RNG) = rng => {
      val (_, next) = rng.nextInt
      (a, next)
    }
    Gen(State(transition))
  }

  def boolean: Gen[Boolean] = {
    val transition: RNG => (Boolean, RNG) = rng => {
      val (int, next) = rng.nextInt
      (int % 2 == 0, next)
    }
    Gen(State(transition))
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence((1 to n).toList.map(_ => g.sample)))

  def intPair(start: Int, stopExclusive: Int): Gen[(Int, Int)] =
    zip(choose(start, stopExclusive), choose(start, stopExclusive))

  def zip[A, B](ga: Gen[A], gb: Gen[B]): Gen[(A, B)] = {
    val transition = for {
      a <- ga.sample
      b <- gb.sample
    } yield (a, b)
    Gen(transition)
  }

  def genStringOfLengthN(n: Int): Gen[String] =
    Gen(listOfN(n, char).sample.map(_.mkString))

  def genOption[A](gen: Gen[A]): Gen[Option[A]] =
    Gen(gen.sample.map(Some(_)))

  def genFlattenOption[A](gen: Gen[Option[A]])(a: A): Gen[A] =
    Gen(gen.sample.map(_.getOrElse(a)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(if (_) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val (gen1, w1) = g1
    val (gen2, w2) = g2
    val normFactor = 1D / (w1 + w2)
    val threshold = w1 * normFactor
    val state = State(RNG.double).flatMap(w => if (w < threshold) gen1.sample else gen2.sample)
    Gen(state)
  }

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n))

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n max 1))

}

case class SGen[A](forSize: Int => Gen[A]) {

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val forSizeB: Int => Gen[B] = n => {
      forSize(n).flatMap(f(_).forSize(n))
    }
    SGen(forSizeB)
  }

  def map[B](f: A => B): SGen[B] =
    SGen(forSize(_).map(f))

}