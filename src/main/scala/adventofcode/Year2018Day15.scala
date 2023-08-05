package adventofcode

import scala.io.Source
import scala.util.{Try, Failure, Success}
import adventofcode.Utils._
import scala.collection.immutable.Queue
import scala.collection.immutable.Queue.EmptyQueue
import scala.annotation.tailrec

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
}

object Grid {
  def parse(src: io.Source): Try[Grid] = {
    def parseLine(line: String) = tryAll(line.map(Tile.from(_))).map(_.toVector)
    tryAll(src.getLines().map(parseLine(_))).map(_.toVector).map(Grid(_))
  }
}

// ============================================================================
// round results

sealed trait Result[T] {
  def value: T
  def flatMap(f: T => Result[T]): Result[T] = this match {
    case Continue(value) => f(value)
    case Finished(value) => Finished(value)
  }
  def map[U](f: T => U): Result[U] = this match {
    case Continue(value) => Continue(f(value))
    case Finished(value) => Finished(f(value))
  }
}
case class Continue[T](value: T) extends Result[T]
case class Finished[T](value: T) extends Result[T]

// ============================================================================
// entity

case class Id(id: Int)

case class Entity(id: Id, typ: EntityType, pos: Pt, hp: Int, ap: Int) {
  type TurnResult = Result[Entities]

  def move(entities: Entities, nbrs: Map[Pt, Iterable[Pt]]): TurnResult = {
    val moving = new MovingEntity(this, entities, nbrs)
    val dest = moving.destination
    dest match {
      case NoTargets     => Finished(entities)
      case NoneReachable => Continue(entities)
      case Stay          => Continue(entities)
      case Dest(pos, dist) =>
        Continue(entities.step(id, moving.step(pos, dist)))
    }
  }

  def attack(entities: Entities): Entities =
    pos.nbrs
      .flatMap(entities.find)
      .filter(_.typ != typ)
      .toVector
      .sortBy(e => (e.hp, e.pos.toTuple))
      .headOption
      .map(target => entities.attack(id, target.id))
      .getOrElse(entities)
}

object Entity {
  // put this here so we don't accidentally use this-bound data
  def takeTurn(
      id: Id,
      entities: Entities,
      nbrs: Map[Pt, Iterable[Pt]]
  ): Result[Entities] =
    // TODO: Entities.get should return an Option
    if (!entities.has(id)) Continue(entities)
    else
      entities.get(id).move(entities, nbrs) match {
        case Continue(moved) => Continue(moved.get(id).attack(moved))
        case Finished(moved) => Finished(moved)
      }
}

class MovingEntity(
    of: Entity,
    entities: Entities,
    nbrs: Map[Pt, Iterable[Pt]]
) extends Entity(of.id, of.typ, of.pos, of.hp, of.ap) {
  def targets: Iterable[Entity] =
    entities.iterator
      .filter(_.typ != typ)
      .toVector
      .sortBy(_.pos.toTuple)

  def candidateDestinations(targets: Iterable[Entity]): Iterable[Pt] =
    targets
      .flatMap(e => nbrs(e.pos))
      .toSet
      .toVector
      .sortBy((p: Pt) => p.toTuple)

  def chooseDestination(dests: Iterable[Pt]): Destination =
    Search
      .dists(pos, dests, nbrs)
      .toVector
      .sortBy(e => (e._2, e._1.toTuple))
      .map(e => Dest(e._1, e._2))
      .headOption
      .getOrElse(NoneReachable)

  def destination: Destination = {
    def inRange(to: Entity): Boolean = pos.adjacent(to.pos)
    if (targets.isEmpty) NoTargets
    else if (targets.find(inRange).isDefined) Stay
    else chooseDestination(candidateDestinations(targets))
  }

  def step(dest: Pt, pathLength: Int): Pt = {
    // do we really need to know all the paths? can we just determine if there
    // is any path of this length from the four neighbors in reading order?
    // TODO: can just call this once
    def hasDist(from: Pt, to: Pt, len: Int) =
      Search.dists(from, List(to), nbrs).get(to).getOrElse(-1) == len
    nbrs(pos).find(nbr => hasDist(nbr, dest, pathLength - 1)).head
  }
}

// ============================================================================
// ecs

case class Entities(
    private val typ: Map[Id, EntityType],
    private val pos: Map[Id, Pt],
    private val byPos: Map[Pt, Id],
    private val hp: Map[Id, Int],
    private val ap: Map[Id, Int]
) {
  def has(id: Id): Boolean = typ.contains(id)
  def get(id: Id): Entity = Entity(id, typ(id), pos(id), hp(id), ap(id))
  def find(pos: Pt): Option[Entity] = byPos.get(pos).map(get)
  def ids: Iterator[Id] = typ.keysIterator
  def iterator: Iterator[Entity] = ids.map(get)
  def toVector: Vector[Entity] = iterator.toVector

  def add(id: Id, typ: EntityType, pos: Pt, hp: Int = 200, ap: Int = 3) =
    new Entities(
      this.typ.updated(id, typ),
      this.pos.updated(id, pos),
      this.byPos.updated(pos, id),
      this.hp.updated(id, hp),
      this.ap.updated(id, ap)
    )

  def remove(id: Id): Entities = {
    val pt = pos(id)
    new Entities(
      typ.removed(id),
      pos.removed(id),
      byPos.removed(pt),
      hp.removed(id),
      ap.removed(id)
    )
  }

  def step(id: Id, to: Pt): Entities = {
    val pt = pos(id)
    copy(pos = pos.updated(id, to), byPos = byPos.removed(pt).updated(to, id))
  }

  def attack(from: Id, to: Id): Entities = {
    val toHp = hp(to) - ap(from)
    if (toHp <= 0) remove(to)
    else copy(hp = hp.updated(to, toHp))
  }

  def update(entity: Entity): Entities = {
    remove(entity.id).add(
      entity.id,
      entity.typ,
      entity.pos,
      entity.hp,
      entity.ap
    )
  }
}

object Entities {
  val empty = new Entities(Map(), Map(), Map(), Map(), Map())

  def fromGrid(grid: Iterable[(Pt, Tile)]): Entities = {
    grid.zipWithIndex.foldLeft(empty) { (em, pair) =>
      pair match {
        case ((pos, Elf), i)    => em.add(Id(i), Elf, pos)
        case ((pos, Goblin), i) => em.add(Id(i), Goblin, pos)
        case _                  => em
      }
    }
  }
}

// ============================================================================
// algorithms

object Search {
  def dists(
      pos: Pt,
      dests: Iterable[Pt],
      nbrs: Map[Pt, Iterable[Pt]]
  ): Map[Pt, Int] =
    Search.bfs(nbrs, Queue((pos, 0)), dests.toSet, Set(pos))

  @tailrec
  def bfs(
      g: Map[Pt, Iterable[Pt]],
      q: Queue[(Pt, Int)],
      dests: Set[Pt],
      v: Set[Pt],
      dists: Map[Pt, Int] = Map()
  ): Map[Pt, Int] = {
    if (dists.size == dests.size) dists
    else if (q.isEmpty) dists
    else {
      val (pos, dist) +: tail = q
      val nbrs = g(pos).filterNot(v.contains).map(pos => (pos, dist + 1))
      val dists1 = if (dests.contains(pos)) dists.updated(pos, dist) else dists
      val q1 = tail ++ nbrs
      val v1 = v.incl(pos) ++ nbrs.map(_._1)
      bfs(g, q1, dests, v1, dists1)
    }
  }
}

// ============================================================================
// game logic

sealed trait Destination
case object Stay extends Destination
case object NoTargets extends Destination
case object NoneReachable extends Destination
case class Dest(pos: Pt, dist: Int) extends Destination

case class Game(
    map: Map[Pt, MapTile],
    entities: Entities,
    height: Int,
    width: Int
) {
  def toGrid: Grid = {
    val tiles = map ++ entities.iterator.map(e => (e.pos, e.typ)).toMap
    val empty: Vector[Vector[Tile]] = Vector.fill(width, height)(Floor)
    val data = tiles.foldLeft(empty) { (data, pair) =>
      data.updated(pair._1.row, data(pair._1.row).updated(pair._1.col, pair._2))
    }
    Grid(data)
  }

  def turnOrder: Iterable[Id] =
    entities.toVector.sortBy(e => e.pos.toTuple).map(_.id)

  def nbrs: Map[Pt, Iterable[Pt]] = {
    def openNbrs(pt: Pt): Iterable[Pt] = pt.nbrs
      .filter(p => p.row >= 0 && p.row < height && p.col >= 0 && p.col < width)
      .filter(p => entities.find(p).isEmpty)
      .filter(p => map.getOrElse(p, Floor) == Floor)
    map.keys.map(pos => (pos, openNbrs(pos))).toMap
  }

  def update(entities: Entities): Game =
    copy(entities = entities)

  def playRound: Result[Game] = {
    val initial: Result[Game] = Continue(this)
    turnOrder
      .foldLeft(initial) { (result, id) =>
        result.flatMap(game =>
          Entity.takeTurn(id, game.entities, game.nbrs).map(update)
        )
      }
  }

  def play: Int = {
    def play(game: Game, round: Int): Int = {
      game.playRound match {
        case Continue(game) => play(game, round + 1)
        case Finished(game) => round * game.entities.iterator.map(_.hp).sum
      }
    }
    play(this, 0)
  }
}

object Game {
  def fromGrid(grid: Grid): Game = {
    val tiles = flatten(grid.data)
    val map = tiles.collect {
      case (pt, Wall) => (pt, Wall)
      case (pt, _)    => (pt, Floor)
    }.toMap
    Game(map, Entities.fromGrid(tiles), grid.data.size, grid.data(0).size)
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
    println(Game.fromGrid(read(args(0)).get).play)
  }
}
