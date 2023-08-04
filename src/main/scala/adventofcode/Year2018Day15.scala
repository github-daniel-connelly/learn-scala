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
    new Entities(
      typ,
      pos.updated(id, to),
      byPos.removed(pt).updated(to, id),
      hp,
      ap
    )
  }

  def attack(from: Id, to: Id): Entities = {
    val toHp = hp(to) - ap(from)
    if (toHp <= 0) remove(to)
    else new Entities(typ, pos, byPos, hp.updated(to, toHp), ap)
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
  def bfs(
      openNbrs: Map[Pt, Iterable[Pt]],
      q: Queue[(Pt, Int)],
      dests: Set[Pt],
      v: Set[Pt],
      dists: Map[Pt, Int]
  ): Map[Pt, Int] =
    q match {
      case (p, d) +: tail => {
        val nbrs = openNbrs(p).filterNot(v.contains).map(p => (p, d + 1))
        val nextDists = if (dests.contains(p)) dists.updated(p, d) else dists
        bfs(openNbrs, tail ++ nbrs, dests, v.incl(p), nextDists)
      }
      case _ => dists
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
        dfs(openNbrs, nbr, dst, dist, v.incl(cur), path :+ nbr, acc)
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

  override def toString: String = {
    entities.iterator
      .foldLeft(toGrid.toString().linesIterator.toSeq) { (lines, e) =>
        lines.updated(e.pos.row, lines(e.pos.row) + s" ${e.typ.ch}(${e.hp})")
      }
      .mkString("\n")
  }

  def turnOrder: Iterable[Id] =
    entities.toVector.sortBy(e => e.pos.toTuple).map(_.id)

  def targets(id: Id): Iterable[Id] = {
    val entity = entities.get(id)
    entities.toVector
      .filter(_.typ != entity.typ)
      .sortBy(_.pos.toTuple)
      .map(_.id)
  }

  def openNbrs(pt: Pt): Iterable[Pt] = pt.nbrs
    .filter(p => p.row >= 0 && p.row < height && p.col >= 0 && p.col < width)
    .filter(p => entities.find(p).isEmpty)
    .filter(p => map.getOrElse(p, Floor) == Floor)

  def allOpenNbrs: Map[Pt, Iterable[Pt]] = {
    map.keys.map(pos => (pos, openNbrs(pos))).toMap
  }

  def candidateDestinations(forTargets: Iterable[Id]): Iterable[Pt] = {
    val nbrs = allOpenNbrs
    forTargets
      .flatMap(e => nbrs(entities.get(e).pos))
      .toSet
      .toVector
      .sortBy((p: Pt) => p.toTuple)
  }

  def inRange(from: Id, to: Id): Boolean =
    entities.get(from).pos.adjacent(entities.get(to).pos)

  def dists(pos: Pt, dests: Iterable[Pt]): Map[Pt, Int] =
    Search.bfs(allOpenNbrs, Queue((pos, 0)), dests.toSet, Set(), Map())

  def chooseDestination(id: Id, dests: Iterable[Pt]): Destination = {
    val entity = entities.get(id)
    dists(entity.pos, dests).toVector
      .sortBy(e => (e._2, e._1.toTuple))
      .map(e => Dest(e._1, e._2))
      .headOption
      .getOrElse(NoneReachable)
  }

  def destination(id: Id): Destination = {
    val targets = this.targets(id)
    if (targets.isEmpty) NoTargets
    else if (targets.find(target => inRange(id, target)).isDefined) Stay
    else chooseDestination(id, candidateDestinations(targets))
  }

  def paths(from: Pt, to: Pt, pathLength: Int): Iterable[Search.Path] =
    Search.dfs(allOpenNbrs, from, to, pathLength, Set(), List(), List())

  def step(from: Pt, to: Pt, pathLength: Int): Pt =
    paths(from, to, pathLength)
      .flatMap(path => path.headOption)
      .toVector
      .sortBy(pt => pt.toTuple)
      .head

  def chooseTarget(id: Id): Option[Id] = {
    val entity = entities.get(id)
    entity.pos.nbrs
      .flatMap(entities.find)
      .filter(_.typ != entity.typ)
      .toVector
      .sortBy(e => (e.hp, e.pos.toTuple))
      .headOption
      .map(_.id)
  }

  def move(id: Id): Result =
    destination(id) match {
      case NoTargets     => Finished(this)
      case NoneReachable => Continue(this)
      case Stay          => Continue(this)
      case Dest(pos, dist) => {
        val to = step(entities.get(id).pos, pos, dist)
        Continue(copy(entities = entities.step(id, to)))
      }
    }

  def attack(id: Id): Game = {
    copy(entities =
      chooseTarget(id)
        .map(target => entities.attack(id, target))
        .getOrElse(entities)
    )
  }

  def takeTurn(id: Id): Result = move(id) match {
    case Continue(game) => Continue(game.attack(id))
    case finished       => finished
  }

  def playRound: Result = {
    val initial: Result = Continue(this)
    turnOrder.foldLeft(initial) { (game, id) =>
      game match {
        case Continue(game) =>
          if (game.entities.has(id)) game.takeTurn(id) else Continue(game)
        case Finished(_) => game
      }
    }
  }

  def hitPoints: Int = entities.hp.values.sum

  def play: (Game, Int) = {
    def play(game: Game, round: Int): (Game, Int) =
      game.playRound match {
        case Continue(game) => play(game, round + 1)
        case Finished(game) => (game, round)
      }
    play(this, 0)
  }

  def results = {
    val (game, round) = play
    val hp = game.hitPoints
    (game, round, hp, round * hp)
  }

  def result = results._4
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
    println(Game.fromGrid(read(args(0)).get))
  }
}
