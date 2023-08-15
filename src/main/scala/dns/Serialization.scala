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

  implicit object ShortSerializer extends Serializer[Short] {
    def serialize(x: Short): ArraySeq[Byte] =
      ArraySeq(x >>> 8, x & 255).map(_.toByte)
  }

  implicit object ByteSerializer extends Serializer[Byte] {
    def serialize(x: Byte): ArraySeq[Byte] = ArraySeq(x)
  }
}
