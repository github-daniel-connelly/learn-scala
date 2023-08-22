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

// TODO: refactor Record to enforce A vs NS and associated data

object Type {
  val A: Short = 1
  val NS: Short = 2
}

object Class {
  val In: Short = 1
}

sealed trait RecordData {
  override def toString(): String = this match {
    case NameServer(name) => name
    case IpAddr(addr)     => addr
    case OpaqueData(data) => s"[opaque data of length ${data.length}"
  }
}
case class IpAddr(addr: String) extends RecordData
case class NameServer(name: String) extends RecordData
case class OpaqueData(data: Array[Byte]) extends RecordData

case class Record(
    name: Name,
    typ: Short,
    cls: Short,
    ttl: Int,
    data: RecordData
)

case class Question(name: Name, typ: Short, cls: Short)

case class Packet(
    header: Header,
    questions: List[Question],
    answers: List[Record],
    authorities: List[Record],
    additionals: List[Record]
)
