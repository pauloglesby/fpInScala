package ly.analogical.fpInScala

import org.scalatest.prop.{Checkers, PropertyChecks}
import org.scalatest.{FunSpec, Matchers}

/**
  * Created by Paul Oglesby on 12/03/2016.
  */
trait BaseSpec extends FunSpec with Matchers with Checkers with PropertyChecks
