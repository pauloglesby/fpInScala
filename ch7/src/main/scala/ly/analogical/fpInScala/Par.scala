package ly.analogical.fpInScala

import java.time.LocalDateTime
import java.time.temporal.TemporalUnit
import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}


object Par {

  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  /**
    * Ex 7.3 Hard
    * Fix the implementation of map2 so that it respects the contract of timeouts on Future
    */
  /*
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get, bf.get))
  }
  */
  private case class Map2Future[A, B, C](fa: Future[A], fb: Future[B], f: (A, B) => C) extends Future[C] {

    /** Model availability of computation as Option; i.e. Some(x) if computation complete, None otherwise */
    @volatile var cache: Option[C] = None

    def isDone = cache.isDefined
    def isCancelled = fa.isCancelled || fb.isCancelled
    def cancel(evenIfRunning: Boolean) = fa.cancel(evenIfRunning) || fb.cancel(evenIfRunning)

    def get: C = compute(Long.MaxValue)
    def get(timeout: Long, units: TimeUnit): C = compute(TimeUnit.NANOSECONDS.convert(timeout, units))

    def compute(timeoutNanos: Long): C = cache match {
      /** If computation already complete, we have the value memoized */
      case Some(c) => c
      /** Otherwise, compute! NB: this implementation does not prevent repeated evaluation... */
      case _ => {
        val start = System.nanoTime
        val a = fa.get(timeoutNanos, TimeUnit.NANOSECONDS)
        val stop = System.nanoTime
        val ta = stop - start
        val b = fb.get(timeoutNanos - ta, TimeUnit.NANOSECONDS)
        val res = f(a, b)
        /** Update cache */
        cache = Some(res)
        res
      }
    }
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
    val (fa, fb) = (a(es), b(es))
    Map2Future(fa, fb, f)
  }

  def fork[A](a: => Par[A]): Par[A] = es =>
    es.submit(new Callable[A] {
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  def run[A](a: Par[A]): A = ???

}
