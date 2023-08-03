package adventofcode

import scala.io.Source
import scala.util.{Try, Failure, Success}
import adventofcode.Utils._
import scala.collection.immutable.Queue
import scala.collection.immutable.Queue.EmptyQueue

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

case class Entity(
    val typ: EntityType,
    val id: Int,
    val pos: Pt,
    hp: Int = 200,
    ap: Int = 3
) {
  def inRange(target: Entity): Boolean = pos.adjacent(target.pos)
}

// TODO: refactor this, we never use id
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
  def contains(pt: Pt): Boolean = byPos.contains(pt)
  def move(entity: Entity, to: Pt): EntityMap =
    remove(entity).add(entity.copy(pos = to))
  def update(entity: Entity): EntityMap =
    add(entity)
}

object EntityMap {
  def apply(entities: Seq[Entity]): EntityMap =
    EntityMap(
      entities.map(e => (e.id, e)).toMap,
      entities.map(e => (e.pos, e)).toMap
    )
}

// TODO: split this up
// TODO: add better language for mutations
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

  def openNbrs(pt: Pt): Iterable[Pt] = pt.nbrs
    .filter(p => p.row >= 0 && p.row < height && p.col >= 0 && p.col < width)
    .filterNot(entities.contains)
    .filter(p => map.getOrElse(p, Floor) == Floor)

  def candidateDestinations(forTargets: Iterable[Entity]): Iterable[Pt] =
    forTargets
      .flatMap(e => openNbrs(e.pos))
      .toSet
      .toVector
      .sortBy((p: Pt) => p.toTuple)

  def bfs(
      q: Queue[(Pt, Int)],
      dests: Set[Pt],
      v: Set[Pt],
      dists: Map[Pt, Int]
  ): Map[Pt, Int] =
    q match {
      case (p, d) +: tail => {
        val nbrs = openNbrs(p).filterNot(v.contains).map(p => (p, d + 1))
        val nextDists = if (dests.contains(p)) dists.updated(p, d) else dists
        bfs(tail ++ nbrs, dests, v.incl(p), nextDists)
      }
      case _ => dists
    }

  def dists(from: Pt, to: Set[Pt]): Map[Pt, Int] =
    bfs(Queue((from, 0)), to, Set(), Map())

  def destination(entity: Entity): Option[(Pt, Int)] = {
    val targets = this.targets(entity)
    if (targets.find(entity.inRange).isDefined) None
    else {
      dists(entity.pos, candidateDestinations(targets).toSet).toVector
        .sortBy(e => (e._2, e._1.toTuple))
        .headOption
    }
  }

  type Path = List[Pt]
  def dfs(
      cur: Pt,
      dst: Pt,
      dist: Int,
      v: Set[Pt],
      path: Path,
      acc: List[Path]
  ): Iterable[Path] =
    if (cur == dst) acc :+ path
    else if (path.length >= dist) acc
    else {
      val nbrs = openNbrs(cur).filterNot(v.contains)
      nbrs.flatMap(nbr => dfs(nbr, dst, dist, v.incl(cur), path :+ nbr, acc))
    }

  def paths(from: Pt, to: Pt, dist: Int): Iterable[Path] =
    dfs(from, to, dist, Set(), List(), List())

  def step(from: Pt, to: Pt, pathLength: Int): Option[Pt] =
    paths(from, to, pathLength)
      .flatMap(path => path.headOption)
      .toVector
      .sortBy(p => p.toTuple)
      .headOption

  def apply(entity: Entity, step: Pt): Game =
    copy(entities = entities.move(entity, step))

  def chooseTarget(entity: Entity): Option[Entity] =
    entity.pos.nbrs
      .flatMap(entities.get)
      .filter(_.typ != entity.typ)
      .toVector
      .sortBy(e => (e.hp, e.pos.toTuple))
      .headOption

  def attack(from: Entity, to: Entity): Game = {
    val updated = to.copy(hp = to.hp - from.ap)
    val updatedEntities =
      if (updated.hp <= 0) entities.remove(updated)
      else entities.update(updated)
    copy(entities = updatedEntities)
  }
}

object Game {
  def fromGrid(grid: Grid): Game = {
    val tiles = flatten(grid.data)
    val map = tiles.collect {
      case (pt, Wall) => (pt, Wall)
      case (pt, _)    => (pt, Floor)
    }
    val entities = tiles.zipWithIndex.collect {
      case ((pt, Elf), id)    => Entity(Elf, id, pt)
      case ((pt, Goblin), id) => Entity(Goblin, id, pt)
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
