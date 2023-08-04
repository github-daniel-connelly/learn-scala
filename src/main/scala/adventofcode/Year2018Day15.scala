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
// entities

case class Id(id: Int)

case class Entity(id: Id, typ: EntityType, pos: Pt, hp: Int, ap: Int)

case class Entities(
    typ: Map[Id, EntityType],
    pos: Map[Id, Pt],
    byPos: Map[Pt, Id],
    hp: Map[Id, Int],
    ap: Map[Id, Int]
) {
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

  def has(id: Id): Boolean = typ.contains(id)
  def get(id: Id): Entity = Entity(id, typ(id), pos(id), hp(id), ap(id))
  def find(pos: Pt): Option[Entity] = byPos.get(pos).map(get)
  def ids: Iterator[Id] = typ.keysIterator
  def iterator: Iterator[Entity] = ids.map(get)
  def toVector: Vector[Entity] = iterator.toVector

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

  def fromGrid(grid: Grid): Entities = {
    flatten(grid.data).zipWithIndex.foldLeft(empty) { (em, pair) =>
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
  @tailrec
  def bfs(
      openNbrs: Map[Pt, Iterable[Pt]],
      q: Queue[(Pt, Int)],
      dests: Set[Pt],
      v: Set[Pt],
      dists: Map[Pt, Int]
  ): Map[Pt, Int] = {
    if (dists.size == dests.size) dists
    else
      q match {
        case (p, d) +: tail => {
          val nbrs = openNbrs(p).filterNot(v.contains).map(p => (p, d + 1))
          val nextDists = if (dests.contains(p)) dists.updated(p, d) else dists
          bfs(
            openNbrs,
            tail.appendedAll(nbrs),
            dests,
            v.incl(p) ++ nbrs.map(_._1),
            nextDists
          )
        }
        case _ => dists
      }
  }

  type Path = List[Pt]
  def dfs(
      openNbrs: Map[Pt, Iterable[Pt]],
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
      nbrs.flatMap(nbr =>
        dfs(
          openNbrs,
          nbr,
          dst,
          dist,
          v.incl(cur) ++ nbrs,
          path :+ nbr,
          acc
        )
      )
    }
}

// ============================================================================
// game logic

sealed trait Destination
case object Stay extends Destination
case object NoTargets extends Destination
case object NoneReachable extends Destination
case class Dest(pos: Pt, dist: Int) extends Destination

sealed trait Result {
  def game: Game
  def get: Game = this match {
    case Continue(game) => game
    case Finished(game) => throw new IllegalStateException("game is over!")
  }
}
case class Continue(game: Game) extends Result
case class Finished(game: Game) extends Result

case class Game(
    map: Map[Pt, MapTile],
    entities: Entities,
    height: Int,
    width: Int
) {
  def toGrid: Grid = {
    // later values clobber earlier, so entities overwrite floors
    val byPos = map ++ entities.iterator.map(e => (e.pos, e.typ))
    val data: Vector[Vector[Tile]] = Vector.fill(height, width)(Floor)
    Grid(byPos.foldLeft(data) { (acc, e) =>
      val (Pt(r, c), t) = e
      acc.updated(r, acc(r).updated(c, t))
    })
  }

  def turnOrder: Iterable[Id] =
    entities.toVector.sortBy(e => e.pos.toTuple).map(_.id)

  def targets(entity: Entity): Iterable[Entity] = {
    entities.iterator
      .filter(_.typ != entity.typ)
      .toVector
      .sortBy(_.pos.toTuple)
  }

  def openNbrs(pt: Pt): Iterable[Pt] = pt.nbrs
    .filter(p => p.row >= 0 && p.row < height && p.col >= 0 && p.col < width)
    .filter(p => entities.find(p).isEmpty)
    .filter(p => map.getOrElse(p, Floor) == Floor)

  // compute this eagerly as we need it several times
  val allOpenNbrs: Map[Pt, Iterable[Pt]] =
    map.keys.map(pos => (pos, openNbrs(pos))).toMap

  def candidateDestinations(forTargets: Iterable[Entity]): Iterable[Pt] = {
    val nbrs = allOpenNbrs
    forTargets
      .flatMap(e => nbrs(e.pos))
      .toSet
      .toVector
      .sortBy((p: Pt) => p.toTuple)
  }

  def inRange(from: Entity, to: Entity): Boolean =
    from.pos.adjacent(to.pos)

  def dists(pos: Pt, dests: Iterable[Pt]): Map[Pt, Int] = {
    val dists =
      Search.bfs(allOpenNbrs, Queue((pos, 0)), dests.toSet, Set(pos), Map())
    dists
  }

  def chooseDestination(entity: Entity, dests: Iterable[Pt]): Destination = {
    dists(entity.pos, dests).toVector
      .sortBy(e => (e._2, e._1.toTuple))
      .map(e => Dest(e._1, e._2))
      .headOption
      .getOrElse(NoneReachable)
  }

  def destination(entity: Entity): Destination = {
    val targets = this.targets(entity)
    if (targets.isEmpty) NoTargets
    else if (targets.find(target => inRange(entity, target)).isDefined) Stay
    else chooseDestination(entity, candidateDestinations(targets))
  }

  def paths(from: Pt, to: Pt, pathLength: Int): Iterable[Search.Path] = {
    val paths =
      Search.dfs(allOpenNbrs, from, to, pathLength, Set(), List(), List())
    paths
  }

  def step(from: Pt, to: Pt, pathLength: Int): Pt = {
    // do we really need to know all the paths? can we just determine if there
    // is any path of this length from the four neighbors in reading order?
    openNbrs(from)
      .find(nbr => dists(nbr, List(to)).get(to).getOrElse(-1) == pathLength - 1)
      .head
  }

  def chooseTarget(entity: Entity): Option[Entity] =
    entity.pos.nbrs
      .flatMap(entities.find)
      .filter(_.typ != entity.typ)
      .toVector
      .sortBy(e => (e.hp, e.pos.toTuple))
      .headOption

  def move(id: Id): Result = {
    val entity = entities.get(id)
    val dest = destination(entity)
    dest match {
      case NoTargets     => Finished(this)
      case NoneReachable => Continue(this)
      case Stay          => Continue(this)
      case Dest(pos, dist) => {
        val to = step(entity.pos, pos, dist)
        Continue(copy(entities = entities.step(id, to)))
      }
    }
  }

  def attack(id: Id): Game = {
    val entity = entities.get(id)
    val target = chooseTarget(entity)
    copy(entities =
      target
        .map(target => entities.attack(id, target.id))
        .getOrElse(entities)
    )
  }

  def takeTurn(id: Id): Result = {
    move(id) match {
      case Continue(game) => Continue(game.attack(id))
      case finished       => finished
    }
  }

  def playRound: Result = {
    val initial: Result = Continue(this)
    turnOrder.foldLeft(initial) { (game, id) =>
      game match {
        case Continue(game) =>
          if (game.entities.has(id)) game.takeTurn(id)
          else Continue(game)
        case Finished(_) => game
      }
    }
  }

  def play: (Game, Int) = {
    def play(game: Game, round: Int): (Game, Int) = {
      game.playRound match {
        case Continue(game) => play(game, round + 1)
        case Finished(game) => (game, round)
      }
    }
    play(this, 0)
  }

  def result = {
    val (game, round) = play
    round * game.entities.hp.values.sum
  }
}

object Game {
  def fromGrid(grid: Grid): Game = {
    val tiles = flatten(grid.data)
    val map = tiles.collect {
      case (pt, Wall) => (pt, Wall)
      case (pt, _)    => (pt, Floor)
    }.toMap
    Game(map, Entities.fromGrid(grid), grid.data.size, grid.data(0).size)
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
    println(Game.fromGrid(read(args(0)).get).result)
  }
}
