package dns

import dns.Packet._
import dns.Encoding._
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.ArraySeq

class EncodingTest extends AnyFunSuite {
  def fromBinary(s: String) = Integer.parseInt(s, 2)

  test("encoding.int") {
    val num = (1 << 30) | 7
    val expected = ArraySeq[Byte](64.toByte, 0, 0, 7)
    assert(num.serialize == expected)
    assert(new Parser(expected.toArray).int.get == num)
  }

  test("encoding.short") {
    val num = fromBinary("101010101").toShort
    val expected = ArraySeq[Byte](1, 85)
    assert(num.serialize == expected)
    assert(new Parser(expected.toArray).short.get == num)
  }

  test("encoding.byte") {
    val num = fromBinary("10101010").toByte
    val expected = ArraySeq[Byte](170.toByte)
    assert(num.serialize == expected)
    assert(new Parser(expected.toArray).byte.get == num)
  }

  test("encoding.name") {
    assert(Name("foo").serialize == ArraySeq[Byte](3, 'f', 'o', 'o', 0))
    assert(Name("foo") == new Parser(Array[Byte](3, 'f', 'o', 'o', 0)).name.get)

    assert(
      Name("foo.com").serialize == ArraySeq[Byte](3, 'f', 'o', 'o', 3, 'c', 'o',
        'm', 0)
    )
    assert(
      Name("foo.com") == new Parser(
        Array[Byte](3, 'f', 'o', 'o', 3, 'c', 'o', 'm', 0)
      ).name.get
    )

    assert(
      Name("tls.a.co").serialize == ArraySeq[Byte](3, 't', 'l', 's', 1, 'a', 2,
        'c', 'o', 0)
    )
    assert(
      Name("tls.a.co") == new Parser(
        Array[Byte](3, 't', 'l', 's', 1, 'a', 2, 'c', 'o', 0)
      ).name.get
    )

    // format: off
    val b = Array[Byte](
      1, 'F',
      3, 'I', 'S', 'I',
      4, 'A', 'R', 'P', 'A',
      0,

      3, 'F', 'O', 'O',
      (3 << 6).toByte, 0, // ptr to "f.isi.arpa"

      (3 << 6).toByte, 6, // ptr to "arpa"
    )
    assert(new Parser(b).name.get == Name("F.ISI.ARPA"))
    assert(new Parser(b, 12).name.get == Name("FOO.F.ISI.ARPA"))
    assert(new Parser(b, 18).name.get == Name("ARPA"))
  }

  test("encoding.header") {
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
    assert(header.serialize == expected)
    assert(header == new Parser(expected.toArray).header.get)
  }

  test("encoding.question") {
    val question = Question(Name("www.jupiter.com"), 257, 9)
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
    assert(question == new Parser(expected.toArray).question.get)
  }

  test("encoding.record.NS") {
    val record = Record(Name("server.net"), Type.NS, 1, 1 << 24, NameServer("a.b.c"))
    val expected = ArraySeq[Byte](6, 's', 'e', 'r', 'v', 'e', 'r', 3, 'n', 'e',
      't', 0, 0, 2, 0, 1, 1, 0, 0, 0, 0, 7, 1, 'a', 1, 'b', 1, 'c', 0)
    assert(record.serialize == expected)
    assert(record == new Parser(expected.toArray).record.get)
  }

  test("encoding.record.A") {
    val record = Record(Name("server.net"), Type.A, 1, 1 << 24, IpAddr("1.2.3.24"))
    val expected = ArraySeq[Byte](6, 's', 'e', 'r', 'v', 'e', 'r', 3, 'n', 'e',
      't', 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 4, 1, 2, 3, 24)
    assert(record.serialize == expected)
    assert(record == new Parser(expected.toArray).record.get)
  }

  test("encoding.packet") {
    val query = Packet(
      Header(
        fromBinary("10000000001").toShort,
        fromBinary("101010101").toShort,
        2, // questions
        2, // answers
        1, // authorities
        1 // additionals
      ),
      List(
        Question(Name("sub.domain.google.com"), 1, 1),
        Question(Name("www.jupiter.com"), 257, 9)
      ),
      List(
        Record(Name("foo.com"), Type.A, 2, 3, IpAddr("8.8.8.8")),
        Record(Name("bar.com"), Type.NS, 5, 6, NameServer("root.net"))
      ),
      List(
        Record(Name("blog.jack.net"), Type.A, 8, 9, IpAddr("1.1.1.1"))
      ),
      List(
        Record(Name("blog.jill.net"), Type.NS, 11, 12, NameServer("root-2.net"))
      )
    )

    // format: off
    val expected = ArraySeq[Byte](
      4, 1, // id
      1, 85, // flags
      0, 2, // numQuestions
      0, 2, // numAnswers
      0, 1, // numAuthorities
      0, 1, // numAdditionals

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
      0, 9, // 0x0009 = 9

      // first answer
      3, 'f', 'o', 'o', 3, 'c', 'o', 'm', 0, 0, 1, 0, 2, 0, 0, 0, 3,
      0, 4, 8, 8, 8, 8, // data
      // second answer
      3, 'b', 'a', 'r', 3, 'c', 'o', 'm', 0, 0, 2, 0, 5, 0, 0, 0, 6,
      0, 10, 4, 'r', 'o', 'o', 't', 3, 'n', 'e', 't', 0,

      // authority
      4, 'b', 'l', 'o', 'g', 4, 'j', 'a', 'c', 'k', 3, 'n', 'e', 't', 0,
      0, 1, 0, 8, 0, 0, 0, 9, 0, 4, 1, 1, 1, 1,

      // additional
      4, 'b', 'l', 'o', 'g', 4, 'j', 'i', 'l', 'l', 3, 'n', 'e', 't', 0,
      0, 2, 0, 11, 0, 0, 0, 12,
      0, 12, 6, 'r', 'o', 'o', 't', '-', '2', 3, 'n', 'e', 't', 0
    )
    assert(query.serialize.zipWithIndex == expected.zipWithIndex)
    assert(query == new Parser(expected.toArray).packet.get)
  }

  test("encoding.recursive") {
    //val q = Packet.recursive("www.example.com", Type.A)
    val q = Packet(
      Header(0, Flags.RecursionDesired, 1, 0, 0, 0),
      List(Question(Name("www.example.com"), Type.A, Class.In)),
      List(), List(), List())
    val r =
      q.copy(header = q.header.copy(id = Integer.parseInt("3c5f", 16).toShort))
    val expected =
      "3c5f0100000100000000000003777777076578616d706c6503636f6d0000010001"
        .grouped(2)
        .map(x => Integer.parseInt(x.toString, 16).toByte)
    assert(r.serialize.toIndexedSeq == expected.toIndexedSeq)
  }
}
