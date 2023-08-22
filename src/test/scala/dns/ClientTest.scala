package dns

import org.scalatest.funsuite.AnyFunSuite

import java.net.DatagramPacket
import java.net.DatagramSocket
import scala.collection.immutable.ArraySeq
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import java.net.InetAddress
import dns.Encoding._
import scala.concurrent.Awaitable

class ClientTest extends AnyFunSuite {
  def receive(socket: DatagramSocket): Future[DatagramPacket] =
    Future {
      val buf = Array.fill(Client.DnsUdpPacketSize)(0.toByte)
      val serverReceived = new DatagramPacket(buf, buf.length)
      socket.receive(serverReceived)
      serverReceived
    }

  def send(
      socket: DatagramSocket,
      addr: InetAddress,
      port: Int,
      data: Array[Byte]
  ): Future[Unit] =
    Future {
      val packet = new DatagramPacket(data, data.length, addr, port)
      socket.send(packet)
    }

  test("resolve") {
    val server = new DatagramSocket
    try {
      val client = new Client("localhost", server.getLocalPort())

      // set up the server handler
      val serverCompletion = receive(server)
        .map(packet => {
          // validate the client sent the right request
          val got = Encoding.parse(packet.getData()).get
          assert(got.questions(0).name.name == "foo.bar")

          // send the response
          val responsePacket = Packet(
            Header(1, 4, 1, 1, 0, 0),
            List(Question(Name("foo.bar"), 1, 2)),
            List(Record(Name("foo.bar"), 1, 2, 3, IpAddr("1.2.3.4"))),
            List(),
            List()
          )
          send(
            server,
            packet.getAddress(),
            packet.getPort(),
            responsePacket.serialize.toArray
          )
        })

      // issue the request and wait
      val clientCompletion = client.resolve("foo.bar")
      Await.result(serverCompletion, 1.seconds)

      // validate the client processed the response
      val response = Await.result(clientCompletion, 1.seconds)
      assert(response == "1.2.3.4")

    } finally {
      server.close()
    }
  }

  test("resolve.server-does-not-exist") {
    // TODO
  }

  test("resolve.server-slow") {
    // TODO
  }

  test("resolve.server-sends-nonsense") {
    // TODO
  }
}
