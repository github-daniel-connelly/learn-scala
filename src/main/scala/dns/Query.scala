package dns

import dns.Serialization._
import scala.collection.immutable.ArraySeq
import scala.util.Random

// DNS query model types
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

case class Question(name: String, typ: Short, cls: Short)

object Question {
  object Type {
    val A: Short = 1
  }

  object Class {
    val In: Short = 1
  }

  implicit object QuestionSerializer extends Serializer[Question] {
    val nul: ArraySeq[Byte] = ArraySeq(0)
    def serialize(question: Question): ArraySeq[Byte] = {
      def segment(s: String): ArraySeq[Byte] =
        s.length.toByte.serialize ++ s.map(_.toByte)
      val segments = ArraySeq.from(question.name.split('.').flatMap(segment))
      segments ++ nul ++ question.typ.serialize ++ question.cls.serialize
    }
  }
}

case class Query(header: Header, questions: List[Question])

object Query {
  def randomID: Short = Random.nextInt(1 << 16).toShort

  def recursive(name: String, typ: Short): Query =
    Query(
      Header(randomID, Header.Flags.RecursionDesired, 1, 0, 0, 0),
      List(Question(name, typ, Question.Class.In))
    )

  implicit object QuerySerializer extends Serializer[Query] {
    def serialize(t: Query): ArraySeq[Byte] =
      t.header.serialize ++ t.questions.flatMap(_.serialize)
  }
}
