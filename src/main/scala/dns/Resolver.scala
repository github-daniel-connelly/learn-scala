package dns

import scala.concurrent.Future
import scala.util.Failure
import scala.concurrent.Promise
import scala.util.Success
import scala.concurrent.ExecutionContext
import scala.util.Try

object Resolver {
  val RootNameServer = "198.41.0.4"
}

case class NoRecordFoundException(name: String)
    extends Exception(s"no record found for $name")
case class UnexpectedAnswerTypeException(typ: Short)
    extends Exception(s"unexpected answer record type: $typ")

class Resolver(client: Client)(implicit ec: ExecutionContext) {
  private def findType[T <: Record](list: List[Record], typ: Short) =
    list.find(_.typ == typ).map(_.asInstanceOf[T])

  def resolve(
      name: String,
      nameserver: String = Resolver.RootNameServer
  ): Future[String] = {
    println(s"resolving $name via $nameserver")
    client
      .query(name, nameserver)
      .flatMap(packet =>
        if (!packet.answers.isEmpty) packet.answers.head match {
          case a: ARecord      => Future.successful(a.addr)
          case cn: CNameRecord => resolve(cn.redirect)
          case r => Future.failed(UnexpectedAnswerTypeException(r.typ))
        }
        else if (!packet.additionals.isEmpty)
          // we know the next server to ask
          findType[ARecord](packet.additionals, Type.A)
            .map(_.addr)
            .map(ns => resolve(name, ns))
            .getOrElse(Future.failed(NoRecordFoundException(name)))
        else
          // we don't know the next server to ask, but we do know the name
          // of that server -- so let's resolve it and then proceed
          findType[NSRecord](packet.authorities, Type.NS)
            .map(record =>
              resolve(record.server).flatMap(ns => resolve(name, ns))
            )
            .getOrElse(Future.failed(NoRecordFoundException(name)))
      )
  }
}
