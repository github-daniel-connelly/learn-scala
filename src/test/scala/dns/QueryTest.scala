package dns

import org.scalatest.funsuite.AnyFunSuite
import dns.Query._
import scala.collection.immutable.ArraySeq

class QueryTest extends AnyFunSuite {
  def fromBinary(s: String) = Integer.parseInt(s, 2)

  test("serialize.short") {
    val num = fromBinary("101010101").toShort
    val expected = ArraySeq[Byte](1, 85)
    assert(num.serialize === expected)
  }

  test("serialize.byte") {
    val num = fromBinary("10101010").toByte
    val expected = ArraySeq[Byte](170.toByte)
    assert(num.serialize == expected)
  }

  test("serialize.header") {
    val header =
      Header(
        fromBinary("10000000001").toShort,
        fromBinary("101010101").toShort,
        2,
        7,
        49,
        3
      )
    val expected =
      ArraySeq[Byte](
        4, 1, // id
        1, 85, // flags
        0, 2, // numQuestions
        0, 7, // numAnswers
        0, 49, // numAuthorities
        0, 3 // numAdditionals
      )
    assert(header.serialize === expected)
  }

  test("serialize.question") {
    val question = Question("www.jupiter.com", 257, 9)
    val expected = ArraySeq[Byte](
      3, // length of www
      'w', 'w', 'w', // www
      7, // length of jupiter
      'j', 'u', 'p', 'i', 't', 'e', 'r', // jupiter
      3, // length of com
      'c', 'o', 'm', // com
      0, // nul
      1, 1, // 0x0101 = 257
      0, 9 // 0x0009 = 9
    )
    assert(question.serialize == expected)
  }
}
