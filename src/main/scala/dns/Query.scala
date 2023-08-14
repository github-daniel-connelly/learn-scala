package dns

import scala.collection.immutable.ArraySeq

object Query {
  // Serializer type class
  trait Serializer[T] {
    def serialize(t: T): ArraySeq[Byte]
  }

  // Extend types in the type class with a |serialize| method
  implicit class SerializerOps[T](value: T) {
    def serialize(implicit serializer: Serializer[T]): ArraySeq[Byte] =
      serializer.serialize(value)
  }

  // DNS query model types
  case class Header(
      id: Short,
      flags: Short,
      numQuestions: Short,
      numAnswers: Short,
      numAuthorities: Short,
      numAdditionals: Short
  )

  case class Question(name: String, typ: Short, cls: Short)

  // Type class implementations for byte, short, Header, and Question

  implicit object ShortSerializer extends Serializer[Short] {
    def serialize(x: Short): ArraySeq[Byte] =
      ArraySeq(x >>> 8, x & 255).map(_.toByte)
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

  implicit object ByteSerializer extends Serializer[Byte] {
    def serialize(x: Byte): ArraySeq[Byte] = ArraySeq(x)
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
