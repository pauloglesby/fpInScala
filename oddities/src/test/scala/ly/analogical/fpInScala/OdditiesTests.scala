package ly.analogical.fpInScala

class OdditiesTests extends BaseWordSpec {

  def firsts(i: Int) = for {
    x <- Stream.from(0)
  } yield (i, x)

  def seconds(i: Int) = for {
    x <- Stream.from(0)
  } yield (x, i)

  val allPairs = {
    for {
      i <- Stream.from(1)
      first <- firsts(i)
      second <- seconds(i)
    } yield first #:: Stream(second)
  }.flatten

  "allPairs" should {

    "produce an infinite stream of all pairs of positive integers s.t. each pair is produced only once" in {
      println(allPairs.take(10).toList)
      println(firsts(0).take(10).toList)

      println((Stream.from(0) zip Stream.from(1)).take(10).toList)
      println(Stream.from(0).flatMap(x => Stream.continually(x) zip Stream.from(0)).take(10).toList)
    }

  }

}
