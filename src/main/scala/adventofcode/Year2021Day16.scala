package adventofcode

import scala.io.Source
import scala.util.{Try, Success, Failure}

case class Header(version: Long, typ: Long)
sealed trait PacketContent
case class Literal(lit: Long) extends PacketContent
case class Operator(operands: Seq[Packet]) extends PacketContent
case class Packet(header: Header, content: PacketContent)

object Year2021Day16 {
  type Bit = Byte
  type Bits = Seq[Bit]

  // TODO: i think parser is supposed to be a monad somehow
  type Parser[T] = (Bits => Try[(T, Bits)])

  private def decodeInt(b: Bits): Long =
    b.foldLeft(0L)((acc, b) => (acc << 1) | b)

  private val parseLiteral: Parser[Literal] = b => {
    val groups = b.grouped(5).toSeq
    val prefix = groups.takeWhile(group => group.head == 1)
    val suffix = groups.drop(prefix.length).take(1)
    val bits = (prefix ++ suffix).flatMap(_.drop(1))
    val int = decodeInt(bits)
    Success(Literal(int), b.drop(5 * (prefix.length + suffix.length)))
  }

  // TODO: parsePackets and parsePacketsN are nearly identical
  // figure out how to unify them
  private val parsePackets: Parser[Seq[Packet]] = b => {
    def parsePackets(b: Bits, len: Long): Try[(Seq[Packet], Bits)] = {
      if (len == 0) Success((Seq.empty, b))
      else
        for {
          (packet, b1) <- parsePacket(b)
          (packets, b1) <- parsePackets(b1, len - (b.length - b1.length))
        } yield (packet +: packets, b1)
    }
    parsePackets(b.drop(15), decodeInt(b.take(15)))
  }

  private val parsePacketsN: Parser[Seq[Packet]] = b => {
    def parsePacketsN(b: Bits, n: Long): Try[(Seq[Packet], Bits)] = {
      if (n == 0) Success((Seq.empty, b))
      else
        for {
          (packet, b) <- parsePacket(b)
          (packets, b) <- parsePacketsN(b, n - 1)
        } yield (packet +: packets, b)
    }
    parsePacketsN(b.drop(11), decodeInt(b.take(11)))
  }

  private val parseOperator: Parser[Operator] = b => {
    val result =
      if (b.head == 0) parsePackets(b.drop(1))
      else parsePacketsN(b.drop(1))
    result.map(pair => (Operator(pair._1), pair._2))
  }

  private val parseHeader: Parser[Header] = b =>
    Try {
      val Seq(v1, v2, v3) = b.take(3)
      val Seq(t1, t2, t3) = b.drop(3).take(3)
      (Header(v1 << 2 | v2 << 1 | v3, t1 << 2 | t2 << 1 | t3), b.drop(6))
    }

  case class InvalidTypeException(typ: Long) extends Exception

  private def contentParser(typ: Long): Try[Parser[PacketContent]] = Try {
    if (typ == 4) parseLiteral(_)
    else if (typ >= 0 && typ <= 7) parseOperator(_)
    else throw InvalidTypeException(typ)
  }

  private val parsePacket: Parser[Packet] = b =>
    for {
      (header, b) <- parseHeader(b)
      parseContent <- contentParser(header.typ)
      (content, b) <- parseContent(b)
    } yield (Packet(header, content), b)

  def parse(b: Bits): Try[Packet] =
    parsePacket(b).map(_._1)

  // takes an 8-bit unsigned value and returns its big-endian bitstring
  def toBits(b: Int): Bits =
    (0 to 7)
      .map(i => (b & (1 << i)) > 0)
      .map(b => if (b) 1 else 0)
      .map(_.toByte)
      .reverse

  // returns a bitstring given a hexstring
  def decode(src: Source): Try[Bits] = {
    src.filter(_ != '\n').grouped(2).foldLeft(Try(Seq.empty[Bit])) {
      (result, pair) =>
        result.flatMap(acc =>
          Try(Integer.parseInt(pair.mkString, 16)).map(b => acc ++ toBits(b))
        )
    }
  }

  def sumVersions(packet: Packet): Long =
    packet.content match {
      case Literal(_)    => packet.header.version
      case Operator(ops) => packet.header.version + ops.map(sumVersions).sum
    }

  def evaluate(packet: Packet): Long =
    packet.content match {
      case Literal(lit) => lit
      case Operator(operands) =>
        (packet.header.typ, operands.map(evaluate)) match {
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
