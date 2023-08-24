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
  val DnsPort = 53
  val DnsUdpPacketSize = 1024
}

class Client(
    val port: Int = Client.DnsPort,
    val shouldLog: Boolean = false
)(implicit
    ec: ExecutionContext
) {

  private def hexdump(bs: Array[Byte]): String = {
    def format(bs: Stream[Byte]) =
      bs.map(b => f"$b%02x").grouped(2).map(_.mkString).mkString(" ")
    val until = bs.lastIndexWhere(_ != 0)
    bs.toStream
      .concat(Stream.continually(0.toByte))
      .take((until / 16 + 1) * 16)
      .zipWithIndex
      .grouped(16)
      .map(group => f"${group.head._2}%04x: ${format(group.map(_._1))}")
      .mkString("\n")
  }

  private def log(s: String): Unit =
    if (shouldLog) println(s)

  private def querySync(host: String, bs: Array[Byte]): Try[Array[Byte]] = {
    val socket = new DatagramSocket
    val result = Try {
      val address = InetAddress.getByName(host)
      log(s"sending:\n${hexdump(bs)}")
      val sendPacket = new DatagramPacket(bs.toArray, bs.length, address, port)
      socket.send(sendPacket)
      val buf = Array.fill(Client.DnsUdpPacketSize)(0.toByte)
      val receivePacket = new DatagramPacket(buf, buf.length)
      socket.receive(receivePacket)
      log(s"received:\n${hexdump(bs)}")
      receivePacket.getData()
    }
    socket.close
    result
  }

  private def query(host: String, bs: Array[Byte]): Future[Array[Byte]] = {
    val promise = Promise[Array[Byte]]()
    Future {
      querySync(host, bs) match {
        case Success(value)     => promise.success(value)
        case Failure(exception) => promise.failure(exception)
      }
    }
    promise.future
  }

  private def query(host: String, q: Packet): Future[Packet] = {
    val promise = Promise[Packet]()
    query(host, q.serialize.toArray).map(Encoding.parse).onComplete {
      case Success(Success(packet))    => promise.success(packet)
      case Success(Failure(exception)) => promise.failure(exception)
      case Failure(exception)          => promise.failure(exception)
    }
    promise.future
  }

  def query(name: String, host: String): Future[Packet] = {
    val q = Packet(
      Header(Random.nextInt(1 << 16).toShort, 0, 1, 0, 0, 0),
      List(Question(Name(name), Type.A, Class.In)),
      List(),
      List(),
      List()
    )
    query(host, q)
  }
}
