package dns

import scala.concurrent.duration._
import scala.collection.immutable.ArraySeq
import java.net.Socket
import scala.concurrent.Promise
import scala.concurrent.Future
import scala.util.Try
import java.io.BufferedWriter
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.OutputStreamWriter
import java.net.DatagramSocket
import java.net.InetAddress
import java.net.DatagramPacket
import scala.util.Success
import scala.util.Failure
import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors

object Client {
  val CloudflareDNSIP = "1.1.1.1"
  val DNSPort = 53
  val DNSUDPPacketSize = 1024
  implicit val ec =
    ExecutionContext.fromExecutor(Executors.newFixedThreadPool(2))

  private def hexdump(bs: Array[Byte]) = {
    def format(bs: Array[Byte]) =
      bs.map(b => f"$b%02x").grouped(2).map(_.mkString).mkString(" ")
    bs.zipWithIndex.grouped(16).foreach { group =>
      println(f"${group.head._2}%04x: ${format(group.map(_._1))}")
    }
  }

  private def sendSync(bs: Array[Byte]): Try[Array[Byte]] = Try {
    val socket = new DatagramSocket
    val address = InetAddress.getByName(CloudflareDNSIP)
    val sendPacket = new DatagramPacket(bs.toArray, bs.length, address, DNSPort)
    socket.send(sendPacket)
    val buf = Array.fill(DNSUDPPacketSize)(0.toByte)
    val receivePacket = new DatagramPacket(buf, buf.length)
    socket.receive(receivePacket)
    receivePacket.getData()
  }

  def send(bs: Array[Byte]): Future[Array[Byte]] = {
    val promise = Promise[Array[Byte]]()
    println("sending datagram:")
    hexdump(bs)
    Future {
      sendSync(bs) match {
        case Success(value) => {
          println("received datagram:")
          hexdump(value.take(value.lastIndexWhere(_ != 0)))
          promise.success(value)
        }
        case Failure(exception) => promise.failure(exception)
      }
    }
    promise.future
  }

  def query(q: Packet): Future[Packet] = {
    val promise = Promise[Packet]()
    println(s"query: $q")
    send(q.serialize.toArray).map(Packet.parse).onComplete {
      case Success(Success(packet))    => promise.success(packet)
      case Success(Failure(exception)) => promise.failure(exception)
      case Failure(exception)          => promise.failure(exception)
    }
    promise.future
  }

  def main(args: Array[String]): Unit = {
    val q = Packet.recursive(args(0), Question.Type.A)
    query(q).onComplete(println)
  }
}
