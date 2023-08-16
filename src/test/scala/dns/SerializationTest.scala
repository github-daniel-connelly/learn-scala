package dns

import org.scalatest.funsuite.AnyFunSuite
import scala.collection.immutable.ArraySeq
import dns.Serialization._

class SerializationTest extends AnyFunSuite {
  def fromBinary(s: String) = Integer.parseInt(s, 2)

  test("serialize.int") {
    val num = (1 << 30) | 7
    val expected = ArraySeq[Byte](64.toByte, 0, 0, 7)
    assert(num.serialize == expected)
  }

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
}
