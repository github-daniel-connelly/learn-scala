package adventofcode

import scala.io.Source
import scala.util.Try
import scala.util.Failure
import scala.util.Success

sealed abstract class Tile(val ch: Char)
case object Wall extends Tile('#')
case object Floor extends Tile('.')
case object Goblin extends Tile('G')
case object Elf extends Tile('E')

case class InvalidTileException(ch: Int) extends Throwable

object Tile {
  def from(ch: Int): Try[Tile] = ch match {
    case '#' => Success(Wall)
    case '.' => Success(Floor)
    case 'G' => Success(Goblin)
    case 'E' => Success(Elf)
    case ch  => Failure(InvalidTileException(ch))
  }
}

case class Grid(rows: Array[Array[Tile]]) {
  override def toString(): String = {
    rows.map(row => row.map(_.ch).mkString).mkString("\n")
  }

  def equals(other: Grid): Boolean =
    other.rows.length == rows.length &&
      other.rows
        .zip(rows)
        .forall(pair => pair._1.sameElements(pair._2))

  override def equals(other: Any): Boolean = other match {
    case other: Grid => equals(other)
    case _           => false
  }
}

object Year2018Day15 {
  def parse(src: io.Source): Try[Grid] = {
    def parseLine(line: String): Try[Array[Tile]] =
      Utils.tryAll(line.map(Tile.from(_))).map(_.toArray)
    Utils.tryAll(src.getLines().map(parseLine(_))).map(_.toArray).map(Grid(_))
  }

  def read(path: String): Try[Grid] = {
    for {
      f <- Try(Source.fromFile(path))
      g <- parse(f)
      _ <- Try(f.close())
    } yield g
  }

  def main(args: Array[String]): Unit = {
    println(read(args(0)).get)
  }
}
