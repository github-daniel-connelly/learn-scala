package dns

import scala.collection.immutable.ArraySeq
import scala.util.Random
import scala.util.Try
import scala.util.Failure
import scala.util.Success

trait Serializer[T] {
  def serialize(t: T): ArraySeq[Byte]
}

object Serializer {
  implicit class SerializerOps[T](value: T) {
    def serialize(implicit serializer: Serializer[T]): ArraySeq[Byte] =
      serializer.serialize(value)
  }

  private def serializeN(
      x: Long,
      numBytes: Int,
      acc: List[Byte] = List.empty
  ): List[Byte] = {
    if (numBytes <= 0) acc
    else serializeN(x >>> 8, numBytes - 1, (x & 255).toByte +: acc)
  }

  implicit object IntSerializer extends Serializer[Int] {
    def serialize(x: Int): ArraySeq[Byte] =
      ArraySeq.from(serializeN(x, 4))
  }

  implicit object ShortSerializer extends Serializer[Short] {
    def serialize(x: Short): ArraySeq[Byte] =
      ArraySeq.from(serializeN(x, 2))
  }

  implicit object ByteSerializer extends Serializer[Byte] {
    def serialize(x: Byte): ArraySeq[Byte] =
      ArraySeq.from(serializeN(x, 1))
  }
}

case class Name(name: String) {
  import Serializer._

  private val nul: Byte = 0

  private def serializeSegment(seg: String) =
    seg.length.toByte.serialize ++ seg.map(_.toByte)

  def serialize: ArraySeq[Byte] =
    ArraySeq.from(name.split('.').flatMap(serializeSegment) :+ nul)
}

case class Record(
    name: Name,
    typ: Short,
    cls: Short,
    ttl: Int,
    data: ArraySeq[Byte]
) {
  import Serializer._

  def serialize: ArraySeq[Byte] =
    name.serialize ++ typ.serialize ++ cls.serialize ++ ttl.serialize ++
      data.length.toShort.serialize ++ data
}

case class Header(
    id: Short,
    flags: Short,
    numQuestions: Short,
    numAnswers: Short,
    numAuthorities: Short,
    numAdditionals: Short
) {
  import Serializer._
  def serialize: ArraySeq[Byte] = ArraySeq(
    id,
    flags,
    numQuestions,
    numAnswers,
    numAuthorities,
    numAdditionals
  ).flatMap(_.serialize)
}

object Header {
  object Flags {
    val RecursionDesired: Short = 1 << 8
  }
}

case class Question(name: Name, typ: Short, cls: Short) {
  import Serializer._
  def serialize: ArraySeq[Byte] =
    name.serialize ++ typ.serialize ++ cls.serialize
}

object Question {
  object Type {
    val A: Short = 1
  }

  object Class {
    val In: Short = 1
  }
}

case class Packet(
    header: Header,
    questions: List[Question],
    answers: List[Record],
    authorities: List[Record],
    additionals: List[Record]
) {
  def serialize: ArraySeq[Byte] =
    header.serialize ++ questions.flatMap(_.serialize) ++ List(
      answers,
      authorities,
      additionals
    ).flatMap(_.flatMap(_.serialize))
}

object Packet {
  def randomID: Short = Random.nextInt(1 << 16).toShort

  def recursive(name: String, typ: Short): Packet =
    Packet(
      Header(randomID, Header.Flags.RecursionDesired, 1, 0, 0, 0),
      List(Question(Name(name), typ, Question.Class.In)),
      List(),
      List(),
      List()
    )

  def parse(b: Array[Byte]): Try[Packet] =
    new Parser(b).packet
}

case class UnexpectedEofException(n: Int)
    extends Exception(s"unexpected eof at $n")

// shout out to recursive descent. see commit history for context
class Parser(b: Array[Byte], var pos: Int = 0) {
  private def advance(n: Int) =
    if (pos + n > b.length) throw UnexpectedEofException(pos)
    else pos += n

  def int: Try[Int] = Try {
    advance(4)
    val (b3, b2, b1, b0) = (b(pos - 4), b(pos - 3), b(pos - 2), b(pos - 1))
    (b3 << 24) | (b2 << 16) | (b1 << 8) | b0
  }

  def short: Try[Short] = Try {
    advance(2)
    ((b(pos - 2) << 8) | b(pos - 1)).toShort
  }

  def byte: Try[Byte] = Try {
    advance(1)
    b(pos - 1)
  }

  private def isEmpty = pos >= b.length

  private def head: Byte =
    if (isEmpty) throw UnexpectedEofException(pos)
    else b(pos)

  private val mask: Byte = (3 << 6).toByte

  private def segments: Try[List[String]] = Try {
    // segment*pointer?
    var list = List.empty[String]
    // segment*
    while (head != 0 && (head & mask) == 0) {
      val len = head
      list = new String(b, pos + 1, len) :: list
      pos += len + 1
    }
    // pointer?
    var ptr: Option[Int] = None
    if (head == 0) {
      pos += 1
    } else {
      assert((head & mask) != 0)
      ptr = Some(((head & (~mask)).toShort << 8) | b(pos + 1))
      pos += 2
    }
    (list, ptr)
  }.flatMap {
    case (segments, None) => Success(segments)
    case (prefix, Some(ptr)) =>
      new Parser(b, ptr).segments.map(suffix => suffix ++ prefix)
  }

  def name: Try[Name] = segments.map(_.reverse.mkString(".")).map(Name)

  def question: Try[Question] = for {
    name <- name
    typ <- short
    cls <- short
  } yield Question(name, typ, cls)

  def record: Try[Record] = {
    val result = for {
      name <- name
      typ <- short
      cls <- short
      ttl <- int
      len <- short
    } yield (
      Record(name, typ, cls, ttl, ArraySeq.from(b.slice(pos, pos + len))),
      len
    )
    result.map {
      case (record, len) => {
        pos += len
        record
      }
    }
  }

  def header: Try[Header] = for {
    id <- short
    flags <- short
    numQuestions <- short
    numAnswers <- short
    numAuthorities <- short
    numAdditionals <- short
  } yield Header(
    id,
    flags,
    numQuestions,
    numAnswers,
    numAuthorities,
    numAdditionals
  )

  def parseList[T](n: Int, parse: () => Try[T]): Try[List[T]] =
    if (n == 0) Success(List.empty[T])
    else parse().flatMap(t => parseList(n - 1, parse).map(tt => t :: tt))

  def packet: Try[Packet] = for {
    header <- header
    questions <- parseList(header.numQuestions, () => question)
    answers <- parseList(header.numAnswers, () => record)
    authorities <- parseList(header.numAuthorities, () => record)
    additionals <- parseList(header.numAdditionals, () => record)
  } yield Packet(header, questions, answers, authorities, additionals)
}
