package adventofcode

import scala.io.Source
import scala.util.{Try, Failure, Success}
import adventofcode.Utils._

// ============================================================================
// basic models

sealed abstract class Tile(val ch: Char)
sealed trait MapTile extends Tile
sealed trait EntityType extends Tile
case object Wall extends Tile('#') with MapTile
case object Floor extends Tile('.') with MapTile
case object Goblin extends Tile('G') with EntityType
case object Elf extends Tile('E') with EntityType

// ============================================================================
// parsing

case class InvalidTileException(ch: Int) extends Exception

object Tile {
  def from(ch: Int): Try[Tile] = ch match {
    case '#' => Success(Wall)
    case '.' => Success(Floor)
    case 'G' => Success(Goblin)
    case 'E' => Success(Elf)
    case ch  => Failure(InvalidTileException(ch))
  }
}

case class Grid(data: Vector[Vector[Tile]]) {
  override def toString(): String = {
    data.map(row => row.map(_.ch).mkString).mkString("\n")
  }

  override def equals(that: Any): Boolean = that match {
    case that: Grid if that.data.length != data.length => false
    case that: Grid => that.data.zip(data).forall(p => p._1.sameElements(p._2))
    case _          => false
  }
}

object Grid {
  def parse(src: io.Source): Try[Grid] = {
    def parseLine(line: String) = tryAll(line.map(Tile.from(_))).map(_.toVector)
    tryAll(src.getLines().map(parseLine(_))).map(_.toVector).map(Grid(_))
  }
}

// ============================================================================
// game representation

sealed trait Entity {
  def id: Int
  def pos: Pt
  def typ: EntityType

  def inRange(target: Entity): Boolean = pos.adjacent(target.pos)
}
case class ElfEntity(id: Int, pos: Pt) extends Entity { val typ = Elf }
case class GoblinEntity(id: Int, pos: Pt) extends Entity { val typ = Goblin }

case class EntityMap(val byId: Map[Int, Entity], val byPos: Map[Pt, Entity]) {
  def size: Int = byId.size
  def add(entity: Entity): EntityMap = EntityMap(
    byId.updated(entity.id, entity),
    byPos.updated(entity.pos, entity)
  )
  def get(id: Int): Option[Entity] = byId.get(id)
  def get(pos: Pt): Option[Entity] = byPos.get(pos)
  def remove(entity: Entity): EntityMap = EntityMap(
    byId.removed(entity.id),
    byPos.removed(entity.pos)
  )
  def toVector: Vector[Entity] = byPos.values.toVector
}

object EntityMap {
  def apply(entities: Seq[Entity]): EntityMap =
    EntityMap(
      entities.map(e => (e.id, e)).toMap,
      entities.map(e => (e.pos, e)).toMap
    )
}

case class Game(
    val map: Map[Pt, MapTile],
    val entities: EntityMap,
    val height: Int,
    val width: Int
) {
  def toGrid: Grid = {
    // later values clobber earlier, so entities overwrite floors
    // invariant: at no point are entities standing on walls
    val byPos = map ++ entities.byPos.map(p => (p._1, p._2.typ))
    val data: Vector[Vector[Tile]] = Vector.fill(height, width)(Floor)
    Grid(byPos.foldLeft(data) { (acc, e) =>
      val (Pt(r, c), t) = e
      acc.updated(r, acc(r).updated(c, t))
    })
  }

  def turnOrder: Iterable[Entity] =
    entities.toVector.sortBy(e => (e.pos.row, e.pos.col))

  def targets(forEntity: Entity): Iterable[Entity] =
    entities.toVector.filter(_.typ != forEntity.typ).sortBy(_.pos.toTuple)

  def candidateDestinations(forTargets: Iterable[Entity]): Iterable[Pt] =
    forTargets
      .flatMap(_.pos.nbrs)
      .toSet
      .filter(p => p.row >= 0 && p.row < height && p.col >= 0 && p.col < width)
      .filter(p => map.getOrElse(p, Floor) == Floor)
      .toVector
      .sortBy(_.toTuple)
}

object Game {
  def fromGrid(grid: Grid): Game = {
    val tiles = flatten(grid.data)
    val map = tiles.collect {
      case (pt, Wall) => (pt, Wall)
      case (pt, _)    => (pt, Floor)
    }
    val entities = tiles.zipWithIndex.collect {
      case ((pt, Elf), id)    => new ElfEntity(id, pt)
      case ((pt, Goblin), id) => new GoblinEntity(id, pt)
    }
    new Game(map.toMap, EntityMap(entities), grid.data.size, grid.data(0).size)
  }
}

// ============================================================================
// play the game

object Year2018Day15 {
  def read(path: String): Try[Grid] = {
    for {
      f <- Try(Source.fromFile(path))
      g <- Grid.parse(f)
      _ <- Try(f.close())
    } yield g
  }

  def main(args: Array[String]): Unit = {
    println(Game.fromGrid(read(args(0)).get).toGrid)
  }
}
