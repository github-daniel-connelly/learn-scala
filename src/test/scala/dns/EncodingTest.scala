package dns

import org.scalatest.funsuite.AnyFunSuite
import scala.collection.immutable.ArraySeq
import dns.Encoding._

class EncodingTest extends AnyFunSuite {
  def fromBinary(s: String) = Integer.parseInt(s, 2)

  test("encoding.int") {
    val num = (1 << 30) | 7
    val expected = ArraySeq[Byte](64.toByte, 0, 0, 7)
    assert(num.serialize == expected)
    val (got, remaining) = deserialize[Int](expected).get
    assert(got == num)
    assert(remaining.isEmpty)
  }

  test("encoding.short") {
    val num = fromBinary("101010101").toShort
    val expected = ArraySeq[Byte](1, 85)
    assert(num.serialize == expected)
    val (got, remaining) = deserialize[Short](expected).get
    assert(got == num)
    assert(remaining.isEmpty)
  }

  test("encoding.byte") {
    val num = fromBinary("10101010").toByte
    val expected = ArraySeq[Byte](170.toByte)
    assert(num.serialize == expected)
    val (got, remaining) = deserialize[Byte](expected).get
    assert(got == num)
    assert(remaining.isEmpty)
  }

  test("encoding.deserialize.partial") {
    val b = ArraySeq[Byte](10, 10, 12, 3)
    val expectedNum = fromBinary("0000101000001010")
    val expectedRemaining = ArraySeq[Byte](12, 3)
    val (got, remaining) = deserialize[Short](b).get
    assert(got == expectedNum)
    assert(remaining == expectedRemaining)
  }

  test("encoding.deserialize.string") {
    val b = ArraySeq[Byte](3, 'I', 'S', 'I', 4, 'A', 'R', 'P', 'A', 0, 4)
    val expected = "ISI.ARPA"
    val expectedRemaining = ArraySeq[Byte](4)
    val (got, remaining) = deserialize[String](b).get
    assert(got === expected)
    assert(remaining === expectedRemaining)
  }
}
