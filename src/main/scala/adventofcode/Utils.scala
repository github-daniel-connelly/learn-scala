package adventofcode

import scala.util.Try

object Utils {
  def tryAll[T](tries: IterableOnce[Try[T]]): Try[Seq[T]] =
    tries.foldLeft(Try(Seq.empty[T])) { (acc, tri) =>
      acc.flatMap(results => tri.map(value => results :+ value))
    }

  case class Pt(row: Int, col: Int) {
    def toTuple: (Int, Int) = (row, col)
    def adjacent(that: Pt): Boolean =
      (row - that.row).abs + (col - that.col).abs == 1
    def nbrs: Seq[Pt] = Seq(
      Pt(row - 1, col),
      Pt(row + 1, col),
      Pt(row, col - 1),
      Pt(row, col + 1)
    )
  }

  def flatten[T](grid: Seq[Seq[T]]): Seq[(Pt, T)] =
    for {
      (row, r) <- grid.zipWithIndex
      (t, c) <- row.zipWithIndex
    } yield (Pt(r, c), t)
}
