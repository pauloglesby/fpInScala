package ly.analogical.fpinscala
package common

import org.scalatest.prop.Checkers
import org.scalatest.{FunSpec, Matchers}

/**
  * Created by Paul Oglesby on 12/03/2016.
  */
trait BaseSpec extends FunSpec with Matchers with Checkers
