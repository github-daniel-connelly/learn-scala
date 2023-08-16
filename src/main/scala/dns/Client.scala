package dns

import dns.Serialization._

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

  private def hexdump(bs: ArraySeq[Byte]) = {
    def format(bs: ArraySeq[Byte]) =
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

  def send(bs: ArraySeq[Byte]): Future[ArraySeq[Byte]] = {
    val promise = Promise[ArraySeq[Byte]]()
    println("sending datagram:")
    hexdump(bs)
    Future {
      sendSync(bs.toArray).map(ArraySeq.from(_)) match {
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

  def query(q: Packet): Unit = {
    println(s"query: $q")
    send(q.serialize).onComplete {
      case Success(value)     => println(s"ok")
      case Failure(exception) => println(s"failure: $exception")
    }
  }

  def main(args: Array[String]): Unit = {
    val q = Packet.recursive(args(0), Question.Type.A)
    query(q)
  }
}
