package adventofcode

import org.scalatest.funsuite.AnyFunSuite
import adventofcode.Year2021Day16._
import scala.io.Source
import scala.util.{Success, Try}

class Year2021Day16Test extends AnyFunSuite {
  def bits(s: String): Seq[Byte] = s.map(_ - '0').map(_.toByte)
  def parse(s: String): Try[Seq[Byte]] =
    Year2021Day16.parse(Source.fromString(s))

  test("parse") {
    assert(parse("D2FE28").get == bits("110100101111111000101000"))
    assert(
      parse("38006F45291200").get == bits(
        "00111000000000000110111101000101001010010001001000000000"
      )
    )
    assert(
      parse("EE00D40C823060").get == bits(
        "11101110000000001101010000001100100000100011000001100000"
      )
    )
  }

  test("parse.newline") {
    assert(parse("D2FE28\n").get == bits("110100101111111000101000"))
  }

  test("parse.invalid") {
    assert(parse("foo").isFailure)
  }
}
