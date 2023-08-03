package adventofcode

import scala.util.Try

object Utils {
  def tryAll[T](tries: IterableOnce[Try[T]]): Try[Seq[T]] =
    tries.foldLeft(Try(Seq.empty[T])) { (acc, tri) =>
      acc.flatMap(results => tri.map(value => results :+ value))
    }

  case class Pt(row: Int, col: Int) {
    def toTuple: (Int, Int) = (row, col)
  }

  def flatten[T](grid: Seq[Seq[T]]): Seq[(Pt, T)] =
    for {
      (row, r) <- grid.zipWithIndex
      (t, c) <- row.zipWithIndex
    } yield (Pt(r, c), t)
}
