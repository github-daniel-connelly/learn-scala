package adventofcode

import scala.io.Source
import scala.util.{Try, Failure, Success}
import adventofcode.Utils._

sealed abstract class Tile(val ch: Char)
case object Wall extends Tile('#')
case object Floor extends Tile('.')
case object Goblin extends Tile('G')
case object Elf extends Tile('E')

case class InvalidTileException(ch: Int) extends Exception

object Tile {
  def apply(ch: Int): Try[Tile] = ch match {
    case '#' => Success(Wall)
    case '.' => Success(Floor)
    case 'G' => Success(Goblin)
    case 'E' => Success(Elf)
    case ch  => Failure(InvalidTileException(ch))
  }
}

case class Grid(rows: Array[Array[Tile]]) {
  override def toString(): String =
    rows.map(_.map(_.ch).mkString).mkString("\n")

  override def equals(that: Any): Boolean = that match {
    case that: Grid if that.rows.length != rows.length => false
    case that: Grid => that.rows.zip(rows).forall(p => p._1.sameElements(p._2))
    case _          => false
  }
}

object Year2018Day15 {
  def parse(src: io.Source): Try[Grid] = {
    def parseLine(line: String) = tryAll(line.map(Tile(_))).map(_.toArray)
    tryAll(src.getLines().map(parseLine(_))).map(_.toArray).map(Grid)
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
