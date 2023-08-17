package dns

import dns.Encoding._
import scala.collection.immutable.ArraySeq
import scala.util.Random
import scala.util.Try
import scala.util.Failure
import scala.util.Success

sealed trait UnresolvedName {
  def resolve(packet: ArraySeq[Byte]): Try[Name] = this match {
    case CompressedName(prefix, offset) => ???
    case UncompressedName(name)         => Success(Name(name))
  }
}

case class CompressedName(prefix: String, offset: Int) extends UnresolvedName
case class UncompressedName(name: String) extends UnresolvedName

object UnresolvedName {
  implicit object UnresolvedNameDeserializer
      extends Deserializer[UnresolvedName] {
    def deserialize(b: ArraySeq[Byte]): Try[(UnresolvedName, ArraySeq[Byte])] =
      Encoding.deserialize[String](b).map { case (s, b) =>
        (UncompressedName(s), b)
      }
  }
}

case class Name(name: String)

object Name {
  val nul: Byte = 0

  implicit object NameSerializer extends Serializer[Name] {
    private def serializeSegment(seg: String) =
      seg.length.toByte.serialize ++ seg.map(_.toByte)
    def serialize(name: Name): ArraySeq[Byte] =
      ArraySeq.from(name.name.split('.').flatMap(serializeSegment) :+ nul)
  }
}

case class Record(
    name: Name,
    typ: Short,
    cls: Short,
    ttl: Int,
    data: ArraySeq[Byte]
)

object Record {
  implicit object RecordSerializer extends Serializer[Record] {
    def serialize(record: Record): ArraySeq[Byte] =
      record.name.serialize ++ record.typ.serialize ++
        record.cls.serialize ++ record.ttl.serialize ++
        record.data.length.toShort.serialize ++ record.data
  }

  implicit object RecordDeserializer extends Deserializer[Record] {
    def deserialize(b: ArraySeq[Byte]): Try[(Record, ArraySeq[Byte])] =
      for {
        (uname, remaining) <- Encoding.deserialize[UnresolvedName](b)
        name <- uname.resolve(b)
        (typ, remaining) <- Encoding.deserialize[Short](remaining)
        (cls, remaining) <- Encoding.deserialize[Short](remaining)
        (ttl, remaining) <- Encoding.deserialize[Int](remaining)
        (n, remaining) <- Encoding.deserialize[Short](remaining)
      } yield (
        Record(name, typ, cls, ttl, remaining.take(n)),
        remaining.drop(n)
      )
  }
}

case class Header(
    id: Short,
    flags: Short,
    numQuestions: Short,
    numAnswers: Short,
    numAuthorities: Short,
    numAdditionals: Short
)

object Header {
  object Flags {
    val RecursionDesired: Short = 1 << 8
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

  implicit object HeaderDeserializer extends Deserializer[Header] {
    def deserialize(b: ArraySeq[Byte]): Try[(Header, ArraySeq[Byte])] =
      if (b.length < 12) Failure(UnexpectedEOFException())
      else
        for {
          (id, b) <- Encoding.deserialize[Short](b)
          (flags, b) <- Encoding.deserialize[Short](b)
          (numQuestions, b) <- Encoding.deserialize[Short](b)
          (numAnswers, b) <- Encoding.deserialize[Short](b)
          (numAuthorities, b) <- Encoding.deserialize[Short](b)
          (numAdditionals, b) <- Encoding.deserialize[Short](b)
        } yield (
          Header(
            id,
            flags,
            numQuestions,
            numAnswers,
            numAuthorities,
            numAdditionals
          ),
          b
        )
  }
}

case class Question(name: Name, typ: Short, cls: Short)

object Question {
  object Type {
    val A: Short = 1
  }

  object Class {
    val In: Short = 1
  }

  implicit object QuestionSerializer extends Serializer[Question] {
    def serialize(question: Question): ArraySeq[Byte] =
      question.name.serialize ++ question.typ.serialize ++ question.cls.serialize
  }

  implicit object QuestionDeserializer extends Deserializer[Question] {
    def deserialize(b: ArraySeq[Byte]): Try[(Question, ArraySeq[Byte])] =
      for {
        (uname, remaining) <- Encoding.deserialize[UnresolvedName](b)
        name <- uname.resolve(b)
        (typ, remaining) <- Encoding.deserialize[Short](remaining)
        (cls, remaining) <- Encoding.deserialize[Short](remaining)
      } yield (Question(name, typ, cls), remaining)
  }
}

case class Packet(
    header: Header,
    questions: List[Question],
    answers: List[Record],
    authorities: List[Record],
    additionals: List[Record]
)

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

  implicit object QuerySerializer extends Serializer[Packet] {
    def serialize(t: Packet): ArraySeq[Byte] =
      t.header.serialize ++ t.questions.flatMap(_.serialize) ++ List(
        t.answers,
        t.authorities,
        t.additionals
      ).flatMap(_.flatMap(_.serialize))
  }

  implicit object PacketDeserializer extends Deserializer[Packet] {
    def deserialize(b: ArraySeq[Byte]): Try[(Packet, ArraySeq[Byte])] =
      for {
        (header, b) <- Encoding.deserialize[Header](b)
        (questions, b) <- deserializeList[Question](b, header.numQuestions)
        (answers, b) <- deserializeList[Record](b, header.numAnswers)
        (authorities, b) <- deserializeList[Record](b, header.numAuthorities)
        (additionals, b) <- deserializeList[Record](b, header.numAdditionals)
      } yield (Packet(header, questions, answers, authorities, additionals), b)
  }
}
