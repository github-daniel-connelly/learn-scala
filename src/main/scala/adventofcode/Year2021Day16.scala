package adventofcode

import scala.io.Source
import scala.util.{Try, Success, Failure}

sealed abstract class Packet(val version: Long, val typ: Long)
case class Literal(override val version: Long, lit: Long)
    extends Packet(version, 4) {
  override def toString = s"$lit"
}
case class Operator(
    override val version: Long,
    override val typ: Long,
    operands: Seq[Packet]
) extends Packet(version, typ) {
  override def toString: String = {
    val op = typ match {
      case 0 => "+"
      case 1 => "*"
      case 2 => "min"
      case 3 => "max"
      case 5 => ">"
      case 6 => "<"
      case 7 => "="
    }
    s"($op ${operands.map(_.toString).mkString(" ")})"
  }
}

object Year2021Day16 {
  type Bit = Byte

  private def decodeInt(b: Seq[Bit]): Long =
    b.foldLeft(0L)((acc, b) => (acc << 1) | b)

  private def parseLiteral(version: Long, b: Seq[Bit]): (Packet, Seq[Bit]) = {
    val groups = b.grouped(5).toSeq
    val prefix = groups.takeWhile(group => group.head == 1)
    val suffix = groups.drop(prefix.length).take(1)
    val bits = (prefix ++ suffix).flatMap(_.drop(1))
    val int = decodeInt(bits)
    (Literal(version, int), b.drop(5 * (prefix.length + suffix.length)))
  }

  private def parsePackets(b: Seq[Bit]): Try[Seq[Packet]] = {
    if (b.length == 0) Success(Seq.empty)
    else
      for {
        (packet, remaining) <- parsePacket(b)
        packets <- parsePackets(remaining)
      } yield packet +: packets
  }

  private def parsePacketsN(
      b: Seq[Bit],
      n: Long
  ): Try[(Seq[Packet], Seq[Bit])] = {
    if (n == 0) Success((Seq.empty, b))
    else
      for {
        (packet, remaining) <- parsePacket(b)
        (packets, remaining) <- parsePacketsN(remaining, n - 1)
      } yield (packet +: packets, remaining)
  }

  private def parseOperator(
      version: Long,
      typ: Long,
      b: Seq[Bit]
  ): Try[(Packet, Seq[Bit])] = Try {
    // TODO: cleanup
    val nlen = if (b.head == 0) 15 else 11
    val n = decodeInt(b.drop(1).take(nlen))
    val afterN = b.drop(nlen + 1)
    val result =
      if (b.head == 0)
        parsePackets(afterN.take(n.toInt)).map(ops =>
          (ops, afterN.drop(n.toInt))
        )
      else parsePacketsN(afterN, n)
    val (ops, remaining) = result.get
    (Operator(version, typ, ops), remaining)
  }

  private def parseHeader(b: Seq[Bit]): Try[(Long, Long, Seq[Bit])] = Try {
    val Seq(v1, v2, v3) = b.take(3)
    val Seq(t1, t2, t3) = b.drop(3).take(3)
    (v1 << 2 | v2 << 1 | v3, t1 << 2 | t2 << 1 | t3, b.drop(6))
  }

  private def parsePacket(b: Seq[Bit]): Try[(Packet, Seq[Bit])] =
    parseHeader(b).flatMap(result =>
      result._2 match {
        case 4   => Success(parseLiteral(result._1, result._3))
        case typ => parseOperator(result._1, typ, result._3)
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

  def sumVersions(packet: Packet): Long = packet match {
    case Literal(version, _)       => version
    case Operator(version, _, ops) => version + ops.map(sumVersions).sum
  }

  def evaluate(packet: Packet): Long =
    packet match {
      case Literal(version, lit) => lit
      case Operator(version, typ, operands) =>
        (typ, operands.map(evaluate)) match {
          case (0, op1 :: ops) => ops.foldLeft(op1)((acc, op) => acc + op)
          case (1, op1 :: ops) => ops.foldLeft(op1)((acc, op) => acc * op)
          case (2, ops)        => ops.min
          case (3, ops)        => ops.max
          case (5, (left :: right :: Nil)) => if (left > right) 1 else 0
          case (6, (left :: right :: Nil)) => if (left < right) 1 else 0
          case (7, (left :: right :: Nil)) => if (left == right) 1 else 0
        }
    }

  def main(args: Array[String]): Unit = {
    val packet = parse(decode(Source.fromFile(args(0))).get).get
    println(sumVersions(packet))
    println(evaluate(packet))
  }
}
