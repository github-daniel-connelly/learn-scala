package dns

import dns.Serialization._
import scala.collection.immutable.ArraySeq
import scala.util.Random

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
    def serialize(question: Question): ArraySeq[Byte] = {
      question.name.serialize ++ question.typ.serialize ++ question.cls.serialize
    }
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
}
