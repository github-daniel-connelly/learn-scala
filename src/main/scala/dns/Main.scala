package dns

import java.util.concurrent.Executors
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object Main {
  def main(args: Array[String]): Unit = {
    val client = new Client
    val addr = Await.result(client.resolve(args(0)), 5.seconds)
    println(addr)
  }
}
