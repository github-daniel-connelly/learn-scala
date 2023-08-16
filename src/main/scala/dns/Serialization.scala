package dns

import scala.collection.immutable.ArraySeq

object Serialization {
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
}
