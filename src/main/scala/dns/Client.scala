package dns

import dns.Encoding._

import java.io.BufferedReader
import java.io.BufferedWriter
import java.io.InputStreamReader
import java.io.OutputStreamWriter
import java.net.DatagramPacket
import java.net.DatagramSocket
import java.net.InetAddress
import java.net.Socket
import java.util.concurrent.Executors
import scala.collection.immutable.ArraySeq
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.duration._
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.util.Random

object Client {
  val RootNameServer = "198.41.0.4"
  val DnsPort = 53
  val DnsUdpPacketSize = 1024
}

case class NoRecordFoundException(name: String)
    extends Exception(s"no record found for $name")

class Client(
    val host: String = Client.RootNameServer,
    val port: Int = Client.DnsPort,
    val logging: Boolean = false
)(implicit
    ec: ExecutionContext
) {

  private def hexdump(bs: Array[Byte]) = {
    def format(bs: Stream[Byte]) =
      bs.map(b => f"$b%02x").grouped(2).map(_.mkString).mkString(" ")
    val until = bs.lastIndexWhere(_ != 0)
    val untilEndOfLine = (until / 16 + 1) * 16
    val data = bs.toStream.concat(Stream.continually(0.toByte))
    data.take(untilEndOfLine).zipWithIndex.grouped(16).foreach { group =>
      println(f"${group.head._2}%04x: ${format(group.map(_._1))}")
    }
  }

  private def querySync(bs: Array[Byte]): Try[Array[Byte]] = {
    val socket = new DatagramSocket
    val result = Try {
      val address = InetAddress.getByName(host)
      if (logging) {
        println("sending data:")
        hexdump(bs)
      }
      val sendPacket = new DatagramPacket(bs.toArray, bs.length, address, port)
      socket.send(sendPacket)
      val buf = Array.fill(Client.DnsUdpPacketSize)(0.toByte)
      val receivePacket = new DatagramPacket(buf, buf.length)
      socket.receive(receivePacket)
      if (logging) {
        println("received data:")
        hexdump(receivePacket.getData())
      }
      receivePacket.getData()
    }
    socket.close
    result
  }

  private def query(bs: Array[Byte]): Future[Array[Byte]] = {
    val promise = Promise[Array[Byte]]()
    Future {
      querySync(bs) match {
        case Success(value)     => promise.success(value)
        case Failure(exception) => promise.failure(exception)
      }
    }
    promise.future
  }

  private def query(q: Packet): Future[Packet] = {
    val promise = Promise[Packet]()
    query(q.serialize.toArray).map(Encoding.parse).onComplete {
      case Success(Success(packet))    => promise.success(packet)
      case Success(Failure(exception)) => promise.failure(exception)
      case Failure(exception)          => promise.failure(exception)
    }
    promise.future
  }

  private def randomID: Short = Random.nextInt(1 << 16).toShort

  def request(name: String, typ: Short, flags: Short = 0): Packet =
    Packet(
      Header(randomID, flags, 1, 0, 0, 0),
      List(Question(Name(name), typ, Class.In)),
      List(),
      List(),
      List()
    )

  def resolve(name: String): Future[String] = {
    val q = request(name, Type.A)
    if (logging) println(s"sending: $q")
    query(q).map(packet => {
      if (logging) println(s"received: $packet")
      packet.answers.headOption match {
        case None         => throw new NoRecordFoundException(name)
        case Some(answer) => answer.toString
      }
    })
  }
}
