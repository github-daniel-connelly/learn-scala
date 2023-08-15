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
  val ip = "1.1.1.1"
  val DNSPort = 53
  val DNSUDPPacketSize = 1024
  implicit val ec =
    ExecutionContext.fromExecutor(Executors.newFixedThreadPool(4))

  private def hexdump(bs: ArraySeq[Byte]) = {
    def format(bs: ArraySeq[Byte]) =
      bs.grouped(2).map(pair => f"${pair(0)}%02x${pair(1)}%02x").mkString(" ")
    bs.zipWithIndex.grouped(16).foreach { group =>
      println(f"${group.head._2}%04x: ${format(group.map(_._1))}")
    }
  }

  private def sendSync(bs: Array[Byte]): Try[Array[Byte]] = Try {
    println("creating datagram")
    val socket = new DatagramSocket
    println("constructing inetaddr...")
    val address = InetAddress.getByName(ip)
    println(s"inetaddr: $address")
    val sendPacket = new DatagramPacket(bs.toArray, bs.length, address, DNSPort)
    println("sending datagram to...")
    socket.send(sendPacket)
    println("sent.")
    val buf = Array.fill(DNSUDPPacketSize)(0.toByte)
    val receivePacket = new DatagramPacket(buf, buf.length)
    println("receiving datagram...")
    socket.receive(receivePacket)
    println("received.")
    receivePacket.getData()
  }

  def send(bs: ArraySeq[Byte]): Future[ArraySeq[Byte]] = {
    val promise = Promise[ArraySeq[Byte]]()
    hexdump(bs)
    Future {
      println("async...")
      sendSync(bs.toArray).map(ArraySeq.from(_)) match {
        case Success(value) => {
          val until = value.lastIndexWhere(_ != 0)
          hexdump(value.take(until))
          promise.success(value)
        }
        case Failure(exception) => promise.failure(exception)
      }
    }
    promise.future
  }

  def query(q: Query): Unit = {
    println(s"query: $q")
    send(q.serialize).onComplete {
      case Success(value)     => println(s"result: $value")
      case Failure(exception) => println(s"failure: $exception")
    }
  }

  def main(args: Array[String]): Unit = {
    val q = Query.recursive(args(0), Question.Type.A)
    query(q)
  }
}
