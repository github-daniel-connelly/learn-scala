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

object Class {
  val In: Short = 1
}

// TODO: make this an enum
object Type {
  val A: Short = 1
  val NS: Short = 2
  val CName: Short = 5
}

sealed abstract class Record(val typ: Short) {
  def name: Name
  def cls: Short
  def ttl: Int
}

case class ARecord(name: Name, cls: Short, ttl: Int, addr: String)
    extends Record(Type.A)

case class NSRecord(name: Name, cls: Short, ttl: Int, server: String)
    extends Record(Type.NS)

case class CNameRecord(name: Name, cls: Short, ttl: Int, redirect: String)
    extends Record(Type.CName)

case class OpaqueRecord(
    name: Name,
    override val typ: Short,
    cls: Short,
    ttl: Int,
    data: Array[Byte]
) extends Record(typ)

case class Question(name: Name, typ: Short, cls: Short)

case class Packet(
    header: Header,
    questions: List[Question],
    answers: List[Record],
    authorities: List[Record],
    additionals: List[Record]
)
