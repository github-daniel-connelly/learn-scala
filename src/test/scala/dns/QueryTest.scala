package dns

import dns.Query._
import dns.Serialization._
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.ArraySeq

class QueryTest extends AnyFunSuite {
  def fromBinary(s: String) = Integer.parseInt(s, 2)

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
    assert(question.serialize === expected)
  }

  test("serialize.query") {
    val query = Query(
      Header(
        fromBinary("10000000001").toShort,
        fromBinary("101010101").toShort,
        2,
        0,
        0,
        0
      ),
      List(
        Question("sub.domain.google.com", 1, 1),
        Question("www.jupiter.com", 257, 9)
      )
    )
    val expected = ArraySeq[Byte](
      4, 1, // id
      1, 85, // flags
      0, 2, // numQuestions
      0, 0, // numAnswers
      0, 0, // numAuthorities
      0, 0, // numAdditionals

      // first question
      3, // length of sub
      's', 'u', 'b', // sub
      6, // length of domain
      'd', 'o', 'm', 'a', 'i', 'n', // domain
      6, // length of google
      'g', 'o', 'o', 'g', 'l', 'e', // google
      3, // length of com
      'c', 'o', 'm', // com
      0, // nul
      0, 1, // 1
      0, 1, // 1

      // second question
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
    assert(query.serialize === expected)
  }
}
