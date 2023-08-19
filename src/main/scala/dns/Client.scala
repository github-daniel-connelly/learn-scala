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

class Client(implicit ec: ExecutionContext) {
  private val Cloudflare = "1.1.1.1"
  private val DnsPort = 53
  private val DnsUdpPacketSize = 1024

  private def querySync(bs: Array[Byte]): Try[Array[Byte]] = {
    val socket = new DatagramSocket
    val result = Try {
      val address = InetAddress.getByName(Cloudflare)
      val sendPacket =
        new DatagramPacket(bs.toArray, bs.length, address, DnsPort)
      socket.send(sendPacket)
      val buf = Array.fill(DnsUdpPacketSize)(0.toByte)
      val receivePacket = new DatagramPacket(buf, buf.length)
      socket.receive(receivePacket)
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

  private def formatAddress(data: ArraySeq[Byte]): String =
    data.map(_ & 0xff).map(b => f"$b%d").mkString(".")

  def resolve(name: String, server: String = Cloudflare): Future[String] = {
    val q = Packet.recursive(name, Type.A)
    query(q).map(packet => formatAddress(packet.answers(0).data))
  }
}
