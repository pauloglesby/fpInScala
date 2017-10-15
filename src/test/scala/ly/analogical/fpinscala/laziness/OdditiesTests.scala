package ly.analogical.fpinscala
package laziness

import common.BaseWordSpec

import collection.immutable.{Stream => st}

class OdditiesTests extends BaseWordSpec {

  def firsts(i: Int) = for {
    x <- st.from(0)
  } yield (i, x)

  def seconds(i: Int) = for {
    x <- st.from(0)
  } yield (x, i)

  val allPairs = {
    for {
      i <- st.from(1)
      first <- firsts(i)
      second <- seconds(i)
    } yield first #:: st.apply(second)
  }.flatten

  "allPairs" should {

    "produce an infinite stream of all pairs of positive integers s.t. each pair is produced only once" in {
      println(allPairs.take(10).toList)
      println(firsts(0).take(10).toList)

      println((st.from(0) zip st.from(1)).take(10).toList)
      println(st.from(0).flatMap(x => st.continually(x) zip st.from(0)).take(10).toList)
    }

  }

}
