package dns

import dns.Serialization._

import scala.collection.immutable.ArraySeq

object Client {
  def main(args: Array[String]): Unit = {
    val q = Query.recursive(args(0), Question.Type.A)
    println(s"query: $q")
    println(s"serialized: [${q.serialize}]]")
  }
}
