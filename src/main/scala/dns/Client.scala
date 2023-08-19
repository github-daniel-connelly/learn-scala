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

object Client {
  val Cloudflare = "1.1.1.1"
  val DnsPort = 53
  val DnsUdpPacketSize = 1024
}

class Client(
    val host: String = Client.Cloudflare,
    val port: Int = Client.DnsPort
)(implicit
    ec: ExecutionContext
) {
  private def querySync(bs: Array[Byte]): Try[Array[Byte]] = {
    val socket = new DatagramSocket
    val result = Try {
      val address = InetAddress.getByName(host)
      val sendPacket = new DatagramPacket(bs.toArray, bs.length, address, port)
      socket.send(sendPacket)
      val buf = Array.fill(Client.DnsUdpPacketSize)(0.toByte)
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

  def resolve(name: String): Future[String] = {
    val q = Packet.recursive(name, Type.A)
    query(q).map(packet => formatAddress(packet.answers(0).data))
  }
}
