package adventofcode

import scala.util.Try

object Utils {
  def tryAll[T](tries: IterableOnce[Try[T]]): Try[Seq[T]] =
    tries.foldLeft(Try(Seq.empty[T])) { (acc, tri) =>
      acc.flatMap(results => tri.map(value => results :+ value))
    }
}
