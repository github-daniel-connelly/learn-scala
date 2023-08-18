package dns

import scala.collection.immutable.ArraySeq
import scala.util.Try
import scala.util.Failure
import scala.util.Success

object Encoding {
  trait Serializer[T] {
    def serialize(t: T): ArraySeq[Byte]
  }

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

  implicit object NameSerializer extends Serializer[Name] {
    private val nul: Byte = 0
    private def serializeSegment(seg: String) =
      seg.length.toByte.serialize ++ seg.map(_.toByte)
    def serialize(name: Name): ArraySeq[Byte] =
      ArraySeq.from(name.name.split('.').flatMap(serializeSegment) :+ nul)
  }

  implicit object HeaderSerializer extends Serializer[Header] {
    def serialize(header: Header): ArraySeq[Byte] = ArraySeq(
      header.id,
      header.flags,
      header.numQuestions,
      header.numAnswers,
      header.numAuthorities,
      header.numAdditionals
    ).flatMap(_.serialize)
  }

  implicit object RecordSerializer extends Serializer[Record] {
    def serialize(record: Record): ArraySeq[Byte] =
      record.name.serialize ++
        record.typ.serialize ++
        record.cls.serialize ++
        record.ttl.serialize ++
        record.data.length.toShort.serialize ++
        record.data
  }

  implicit object QuestionSerializer extends Serializer[Question] {
    def serialize(question: Question): ArraySeq[Byte] =
      question.name.serialize ++ question.typ.serialize ++ question.cls.serialize
  }

  implicit object PacketSerializer extends Serializer[Packet] {
    def serialize(packet: Packet): ArraySeq[Byte] =
      packet.header.serialize ++
        packet.questions.flatMap(_.serialize) ++
        List(
          packet.answers,
          packet.authorities,
          packet.additionals
        ).flatMap(_.flatMap(_.serialize))
  }

  case class UnexpectedEofException(n: Int)
      extends Exception(s"unexpected eof at $n")

  // shout out to recursive descent. see commit history for context
  // TODO: figure out how to make this more functional again without
  // it being horrible
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

  def parse(b: Array[Byte]): Try[Packet] =
    new Parser(b).packet
}
