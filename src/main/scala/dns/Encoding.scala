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

  implicit object NSRecordSerializer extends Serializer[NSRecord] {
    def serialize(t: NSRecord): ArraySeq[Byte] = {
      val server = Name(t.server).serialize
      t.name.serialize ++
        t.typ.serialize ++
        t.cls.serialize ++
        t.ttl.serialize ++ server.length.toShort.serialize ++
        server
    }
  }

  implicit object ARecordSerializer extends Serializer[ARecord] {
    def serializeAddr(addr: String): ArraySeq[Byte] =
      ArraySeq[Byte](0, 4) ++ ArraySeq.from(
        addr.split('.').map(Integer.parseInt).map(_.toByte)
      )

    def serialize(t: ARecord): ArraySeq[Byte] =
      t.name.serialize ++
        t.typ.serialize ++
        t.cls.serialize ++
        t.ttl.serialize ++
        serializeAddr(t.addr)
  }

  implicit object OpaqueRecordSerializer extends Serializer[OpaqueRecord] {
    def serialize(t: OpaqueRecord): ArraySeq[Byte] = ???
  }

  implicit object RecordSerializer extends Serializer[Record] {
    def serialize(record: Record): ArraySeq[Byte] = record match {
      case a @ ARecord(_, _, _, _)              => a.serialize
      case ns @ NSRecord(_, _, _, _)            => ns.serialize
      case opaque @ OpaqueRecord(_, _, _, _, _) => opaque.serialize
    }
  }

  private def formatAddress(data: ArraySeq[Byte]): String =
    data.map(_ & 0xff).map(b => f"$b%d").mkString(".")

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

  case class InvalidRecordType(typ: Int)
      extends Exception(s"invalid record type: $typ")

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

    // ugh
    private def widen(byte: Byte): Int =
      byte.toInt & 0xff

    private def segments: Try[List[String]] = Try {
      // segment*pointer?
      var list = List.empty[String]
      // segment*
      while (head != 0 && (head & mask) == 0) {
        val len: Int = head
        list = new String(b, pos + 1, len) :: list
        pos += len + 1
      }
      // pointer?
      var ptr: Option[Int] = None
      if (head == 0) {
        pos += 1
      } else {
        assert((head & mask) != 0)
        ptr = Some(((widen(head) & (~widen(mask))) << 8) | widen(b(pos + 1)))
        pos += 2
      }
      (list, ptr)
    }.flatMap {
      case (segs, None) => Success(segs)
      case (prefix, Some(ptr)) =>
        new Parser(b, ptr).segments.map(suffix => suffix ++ prefix)
    }

    def name: Try[Name] = segments.map(_.reverse.mkString(".")).map(Name)

    def question: Try[Question] = for {
      name <- name
      typ <- short
      cls <- short
    } yield Question(name, typ, cls)

    /*
    def recordData(typ: Short, len: Short): Try[RecordData] = Try(typ match {
      case Type.A => {
      }
      case Type.NS => NameServer(name.get.name)
      case typ => {
      }
    })
        len <- short
        data <- recordData(typ, len)
      } yield Record(name, typ, cls, ttl, data)
    }
     */

    def record(name: Name, typ: Short, cls: Short, ttl: Int): Try[Record] =
      short.flatMap(len =>
        typ match {
          case Type.A =>
            advance(len)
            val addr = formatAddress(ArraySeq.from(b.slice(pos - len, pos)))
            Success(ARecord(name, cls, ttl, addr))
          case Type.NS => {
            this.name.map(server => NSRecord(name, cls, ttl, server.name))
          }
          case typ => {
            advance(len)
            Success(OpaqueRecord(name, typ, cls, ttl, b.slice(pos - len, pos)))
          }
        }
      )

    def record: Try[Record] =
      for {
        name <- name
        typ <- short
        cls <- short
        ttl <- int
        record <- record(name, typ, cls, ttl)
      } yield record

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
