package adventofcode

import scala.io.Source
import scala.util.{Try, Success, Failure}

object Year2021Day16 {
  // takes an 8-bit unsigned value and returns its big-endian bitstring
  def toBits(b: Int): Seq[Byte] =
    (0 to 7)
      .map(i => (b & (1 << i)) > 0)
      .map(b => if (b) 1 else 0)
      .map(_.toByte)
      .reverse

  // returns a bitstring given a hexstring
  def parse(src: Source): Try[Seq[Byte]] = {
    src.filter(_ != '\n').grouped(2).foldLeft(Try(Seq.empty[Byte])) {
      (result, pair) =>
        result.flatMap(acc =>
          Try(Integer.parseInt(pair.mkString, 16)).map(b => acc ++ toBits(b))
        )
    }
  }

  def main(args: Array[String]): Unit = {
    val data = parse(Source.fromFile(args(0))).get
    println(data)
  }
}
