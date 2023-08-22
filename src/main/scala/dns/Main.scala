package dns

import java.util.concurrent.Executors
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Success
import scala.util.Failure

object Main {
  def resolve(name: String, log: Boolean) = {
    val client = new Client(logging = log)
    client.resolve(name).onComplete {
      case Success(value) => println(value)
      case Failure(NoRecordFoundException(_)) =>
        println(s"no record found for: $name")
      case Failure(exception) => exception.printStackTrace()
    }
  }

  def main(args: Array[String]): Unit = args.toList match {
    case "--log" :: name :: Nil => resolve(name, true)
    case name :: Nil            => resolve(name, false)
    case _                      => println("usage: dns [--log] name")
  }
}
