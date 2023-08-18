package dns

import dns.Encoding._
import scala.collection.immutable.ArraySeq
import scala.util.Random
import scala.util.Try
import scala.util.Failure
import scala.util.Success

object Flags {
  val RecursionDesired: Short = 1 << 8
}

case class Header(
    id: Short,
    flags: Short,
    numQuestions: Short,
    numAnswers: Short,
    numAuthorities: Short,
    numAdditionals: Short
)

case class Name(name: String)

object Type {
  val A: Short = 1
}

object Class {
  val In: Short = 1
}

case class Record(
    name: Name,
    typ: Short,
    cls: Short,
    ttl: Int,
    data: ArraySeq[Byte]
)

case class Question(name: Name, typ: Short, cls: Short)

case class Packet(
    header: Header,
    questions: List[Question],
    answers: List[Record],
    authorities: List[Record],
    additionals: List[Record]
)

object Packet {
  private def randomID: Short = Random.nextInt(1 << 16).toShort

  def recursive(name: String, typ: Short): Packet =
    Packet(
      Header(randomID, Flags.RecursionDesired, 1, 0, 0, 0),
      List(Question(Name(name), typ, Class.In)),
      List(),
      List(),
      List()
    )
}
