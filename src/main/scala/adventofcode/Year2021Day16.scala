package adventofcode

import scala.io.Source
import scala.util.{Try, Success, Failure}

sealed abstract class Packet(val version: Int)
case class Literal(override val version: Int, lit: Int) extends Packet(version)

object Year2021Day16 {
  type Bit = Byte

  def decodeInt(b: Seq[Bit]): Int =
    b.foldLeft(0)((acc, b) => (acc << 1) | b)

  def parseLiteral(version: Int, b: Seq[Bit]): (Packet, Seq[Bit]) = {
    val groups = b.grouped(5).toSeq
    val prefix = groups.takeWhile(group => group.head == 1)
    val suffix = groups.drop(prefix.length).take(1)
    val bits = (prefix ++ suffix).flatMap(_.drop(1))
    val int = decodeInt(bits)
    (Literal(version, int), b.drop(5 * bits.length))
  }

  def parseHeader(b: Seq[Bit]): Try[(Int, Int, Seq[Bit])] = Try {
    val Seq(v1, v2, v3) = b.take(3)
    val Seq(t1, t2, t3) = b.drop(3).take(3)
    (v1 << 2 | v2 << 1 | v3, t1 << 2 | t2 << 1 | t3, b.drop(6))
  }

  def parsePacket(b: Seq[Bit]): Try[(Packet, Seq[Bit])] =
    parseHeader(b).map(result =>
      result._2 match {
        case 4 => parseLiteral(result._1, result._3)
      }
    )

  def parse(b: Seq[Bit]): Try[Packet] = parsePacket(b).map(_._1)

  // takes an 8-bit unsigned value and returns its big-endian bitstring
  def toBits(b: Int): Seq[Bit] =
    (0 to 7)
      .map(i => (b & (1 << i)) > 0)
      .map(b => if (b) 1 else 0)
      .map(_.toByte)
      .reverse

  // returns a bitstring given a hexstring
  def decode(src: Source): Try[Seq[Bit]] = {
    src.filter(_ != '\n').grouped(2).foldLeft(Try(Seq.empty[Bit])) {
      (result, pair) =>
        result.flatMap(acc =>
          Try(Integer.parseInt(pair.mkString, 16)).map(b => acc ++ toBits(b))
        )
    }
  }

  def main(args: Array[String]): Unit = {
    val data = decode(Source.fromFile(args(0))).get
    println(data)
  }
}
