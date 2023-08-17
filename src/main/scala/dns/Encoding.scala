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

  trait Deserializer[T] {
    def deserialize(b: ArraySeq[Byte]): Try[(T, ArraySeq[Byte])]
  }

  case class UnexpectedEOFException() extends Exception("unexpected eof")
  case class UnprocessedDataException()
      extends Exception("data remaining after deserialization")

  // ideally this would be generic, but Numeric[T] doesn't support bitwise ops
  def deserializeN(
      b: ArraySeq[Byte],
      n: Int,
      acc: Long = 0
  ): Try[(Long, ArraySeq[Byte])] =
    if (n == 0) Success((acc, b))
    else if (b.isEmpty) Failure(UnexpectedEOFException())
    else deserializeN(b.tail, n - 1, (acc << 8) | b.head)

  def deserialize[T](b: ArraySeq[Byte])(implicit
      deserializer: Deserializer[T]
  ): Try[(T, ArraySeq[Byte])] =
    deserializer.deserialize(b)

  implicit object IntDeserializer extends Deserializer[Int] {
    def deserialize(b: ArraySeq[Byte]): Try[(Int, ArraySeq[Byte])] =
      deserializeN(b, 4).map(pair => (pair._1.toInt, pair._2))
  }

  implicit object ShortDeserializer extends Deserializer[Short] {
    def deserialize(b: ArraySeq[Byte]): Try[(Short, ArraySeq[Byte])] =
      deserializeN(b, 2).map(pair => (pair._1.toShort, pair._2))
  }

  implicit object ByteDeserializer extends Deserializer[Byte] {
    def deserialize(b: ArraySeq[Byte]): Try[(Byte, ArraySeq[Byte])] =
      deserializeN(b, 1).map(pair => (pair._1.toByte, pair._2))
  }

  implicit object StringDeserializer extends Deserializer[String] {
    private def deserialize(
        b: ArraySeq[Byte],
        acc: List[String]
    ): Try[(String, ArraySeq[Byte])] = {
      if (b.isEmpty) Failure(UnexpectedEOFException())
      else if (b.head == 0) Success(acc.reverseIterator.mkString("."), b.tail)
      else if (b.head > b.tail.length - 1) Failure(UnexpectedEOFException())
      else {
        val segment = b.tail.take(b.head)
        val remaining = b.tail.drop(b.head)
        deserialize(remaining, segment.map(_.toChar).mkString :: acc)
      }
    }

    def deserialize(b: ArraySeq[Byte]): Try[(String, ArraySeq[Byte])] =
      deserialize(b, List.empty)
  }

  def deserializeList[T: Deserializer](
      b: ArraySeq[Byte],
      n: Int,
      acc: List[T] = List.empty
  ): Try[(List[T], ArraySeq[Byte])] =
    if (n == 0) Success((acc.reverse, b))
    else
      for {
        (t, b) <- Encoding.deserialize[T](b)
        (ts, b) <- deserializeList(b, n - 1, t :: acc)
      } yield (ts, b)
}
