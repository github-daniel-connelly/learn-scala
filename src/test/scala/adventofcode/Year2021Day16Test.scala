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

  test("parse.operator.totalLength") {
    assert(
      parse("38006F45291200").get ==
        Operator(1, 6, Vector(Literal(6, 10), Literal(2, 20)))
    )
  }

  test("parse.operator.numPackets") {
    assert(
      parse("EE00D40C823060").get ==
        Operator(7, 3, Vector(Literal(2, 1), Literal(4, 2), Literal(1, 3)))
    )
  }

  test("sumVersions") {
    assert(sumVersions(parse("8A004A801A8002F478").get) == 16)
    assert(sumVersions(parse("620080001611562C8802118E34").get) == 12)
    assert(sumVersions(parse("C0015000016115A2E0802F182340").get) == 23)
    assert(sumVersions(parse("A0016C880162017C3686B18A3D4780").get) == 31)
  }

  test("evaluate") {
    assert(evaluate(parse("C200B40A82").get) == 3)
    assert(evaluate(parse("04005AC33890").get) == 54)
    assert(evaluate(parse("880086C3E88112").get) == 7)
    assert(evaluate(parse("CE00C43D881120").get) == 9)
    assert(evaluate(parse("D8005AC2A8F0").get) == 1)
    assert(evaluate(parse("F600BC2D8F").get) == 0)
    assert(evaluate(parse("9C005AC2F8F0").get) == 0)
    assert(evaluate(parse("9C0141080250320F1802104A08").get) == 1)
  }
}
