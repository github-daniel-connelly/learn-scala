package adventofcode

import org.scalatest.funsuite.AnyFunSuite
import adventofcode.Year2021Day16._
import scala.io.Source
import scala.util.{Success, Try}

class Year2021Day16Test extends AnyFunSuite {
  def bits(s: String): Seq[Byte] = s.map(_ - '0').map(_.toByte)
  def decode(s: String) = Year2021Day16.decode(Source.fromString(s))
  def parse(s: String) = Year2021Day16.parse(decode(s).get)

  test("decode") {
    assert(decode("D2FE28").get == bits("110100101111111000101000"))
    assert(
      decode("38006F45291200").get == bits(
        "00111000000000000110111101000101001010010001001000000000"
      )
    )
    assert(
      decode("EE00D40C823060").get == bits(
        "11101110000000001101010000001100100000100011000001100000"
      )
    )
  }

  test("decode.newline") {
    assert(decode("D2FE28\n").get == bits("110100101111111000101000"))
  }

  test("decode.invalid") {
    assert(decode("foo").isFailure)
  }

  test("parse.invalid") {
    assert(Year2021Day16.parse(bits("10")).isFailure)
  }

  test("parse.literal") {
    assert(parse("D2FE28").get == Literal(6, 2021))
  }
}
