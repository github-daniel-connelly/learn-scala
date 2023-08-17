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
}
