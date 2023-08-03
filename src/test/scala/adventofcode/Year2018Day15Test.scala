package adventofcode

import org.scalatest.funsuite.AnyFunSuite
import scala.io.Source
import adventofcode.Year2018Day15._
import adventofcode.Utils._

class Year2018Day15Test extends AnyFunSuite {
  def parseGame(s: String): Game =
    Game.fromGrid(Grid.parse(Source.fromString(trimLinesLeft(s))).get)

  def trimLinesLeft(s: String): String =
    s.linesIterator.map(_.trim).filterNot(_.isEmpty).mkString("\n")

  def stepAll(game: Game): Game =
    game.turnOrder.foldLeft(game) { (game, entity) =>
      game
        .destination(entity)
        .flatMap(where => game.step(entity.pos, where._1, where._2))
        .map(game.applyStep(entity, _))
        .getOrElse(game)
    }

  test("grid.parsing") {
    val src = Source.fromString("#.EG\nG.E#\n")
    val grid = Grid.parse(src)
    assert(grid.isSuccess)

    val expected = Grid(
      Vector(
        Vector(Wall, Floor, Elf, Goblin),
        Vector(Goblin, Floor, Elf, Wall)
      )
    )
    assert(grid.get == expected)
  }

  test("grid.parsing.failure") {
    val src = Source.fromString("foo")
    val grid = Grid.parse(src)
    assert(grid.isFailure)
  }

  test("entitymap") {
    var m: Map[Pt, Entity] = Map()
    val e = Entity(Elf, Pt(7, 3))
    m = m.updated(e.pos, e)
    assert(m.size == 1)
    assert(m.get(Pt(-1, -7)).isEmpty)
    assert(m.get(Pt(7, 3)).get == e)

    m = m.removed(e.pos)
    assert(m.size == 0)
    assert(m.get(Pt(7, 3)).isEmpty)
  }

  test("game.fromGrid") {
    val grid = Grid(
      Vector(
        Vector(Wall, Floor, Elf, Goblin),
        Vector(Goblin, Floor, Elf, Wall)
      )
    )
    val game = Game.fromGrid(grid)
    assert(game.toGrid == grid)

    assert(game.map.get(Pt(-1, -7)).isEmpty)
    assert(game.map.get(Pt(1, 2)).get == Floor)
  }

  test("game.turnOrder") {
    val game = parseGame("#.EG\nG.E#\n")
    val turn = game.turnOrder.map(e => e.pos)
    val expected = Vector(Pt(0, 2), Pt(0, 3), Pt(1, 0), Pt(1, 2))
    assert(turn.sameElements(expected))
  }

  test("game.targets") {
    val game = parseGame("#.EG\nG.E#\n")
    val elf2 = game.entities.get(Pt(1, 2)).get
    val targetsForElf = game.targets(elf2)
    val goblin1 = game.entities.get(Pt(0, 3)).get
    val targetsForGoblin = game.targets(goblin1)
    val elves = game.entities.values.filter(e => e.typ == Elf)
    val goblins = game.entities.values.filter(e => e.typ == Goblin)
    assert(targetsForGoblin == elves.toVector.sortBy(_.pos.toTuple))
    assert(targetsForElf == goblins.toVector.sortBy(_.pos.toTuple))
  }

  test("game.inRange") {
    val game = parseGame("#.EG\nG.E#\n")

    val goblin2 = game.entities.get(Pt(1, 0)).get
    val inRangeOfGoblin2 = game.entities.values.filter(goblin2.inRange)
    assert(inRangeOfGoblin2.isEmpty)

    val elf1 = game.entities.get(Pt(0, 2)).get
    val inRangeOfElf1 = game.entities.values.filter(elf1.inRange)
    val expected =
      Vector(game.entities.get(Pt(0, 3)).get, game.entities.get(Pt(1, 2)).get)
    assert(inRangeOfElf1 == expected)
  }

  test("game.destinations") {
    val game = parseGame("#..E\nG.E#\n")
    val goblin = game.entities.get(Pt(1, 0)).get
    val dests = game.candidateDestinations(game.targets(goblin))
    val expected = Vector(Pt(0, 2), Pt(1, 1))
    assert(dests == expected)
  }

  def get(game: Game, pts: Iterable[Pt]): Iterable[Entity] =
    pts.map(game.entities.get(_).get)

  test("game.destination") {
    val game = parseGame("#######\n#E..G.#\n#...#.#\n#.G.#G#\n#######\n")
    val elf = game.entities.get(Pt(1, 1)).get

    val targets = game.targets(elf)
    val expectedTargets = get(game, Seq(Pt(1, 4), Pt(3, 2), Pt(3, 5)))
    assert(targets == expectedTargets)

    val destinations = game.candidateDestinations(targets)
    val expectedDestinations = Seq(
      Pt(1, 3),
      Pt(1, 5),
      Pt(2, 2),
      Pt(2, 5),
      Pt(3, 1),
      Pt(3, 3)
    )
    assert(destinations == expectedDestinations)

    val dists = game.dists(elf.pos, destinations.toSet)
    val expectedDists = Map(
      (Pt(1, 3), 2),
      (Pt(2, 2), 2),
      (Pt(3, 1), 2),
      (Pt(3, 3), 4)
    )
    assert(dists == expectedDists)

    val (chosen, dist) = game.destination(elf).get
    assert(chosen == Pt(1, 3))
    assert(dist == 2)
  }

  test("game.destination.none") {
    val game = parseGame("#.EG\nG.E#\n")
    val elf1 = game.entities.get(Pt(0, 2)).get
    assert(game.destination(elf1).isEmpty)
  }

  test("game.paths") {
    val game = parseGame("#######\n#.E...#\n#.....#\n#...G.#\n#######")
    val elf = game.entities.get(Pt(1, 2)).get
    val (dst, dist) = game.destination(elf).get
    assert(dst == Pt(2, 4))
    assert(dist == 3)
    val paths = game.paths(elf.pos, dst, dist)
    val expected = Set(
      List(Pt(2, 2), Pt(2, 3), Pt(2, 4)),
      List(Pt(1, 3), Pt(2, 3), Pt(2, 4)),
      List(Pt(1, 3), Pt(1, 4), Pt(2, 4))
    )
    assert(paths.toSet == expected)
    assert(game.step(elf.pos, dst, dist).get == Pt(1, 3))
  }

  test("game.movement") {
    val game = parseGame("""
      #########
      #G..G..G#
      #.......#
      #.......#
      #G..E..G#
      #.......#
      #.......#
      #G..G..G#
      #########""")
    val result = (1 to 4).foldLeft(game) { (game, _) => stepAll(game) }
    val expected = parseGame("""
      #########
      #.......#
      #..GGG..#
      #..GEG..#
      #G..G...#
      #......G#
      #.......#
      #.......#
      #########""")
    assert(expected.toGrid == result.toGrid)
  }

  test("game.chooseTarget") {
    val start = parseGame("""
G....
..G..
..EG.
..G..
...G.""")
    val game = start.copy(entities =
      start.entities
        .updated(Pt(1, 2), start.entities.get(Pt(1, 2)).get.copy(hp = 4))
        .updated(Pt(2, 3), start.entities.get(Pt(2, 3)).get.copy(hp = 2))
        .updated(Pt(3, 2), start.entities.get(Pt(3, 2)).get.copy(hp = 2))
    )
    val elf = game.entities.get(Pt(2, 2)).get
    val target = game.chooseTarget(elf).get
    assert(target == game.entities.get(Pt(2, 3)).get)
  }

  test("game.chooseTarget.none") {
    val game = parseGame("""
G....
..G..
..EG.
..G..
...G.""")
    val goblin = game.entities.get(Pt(0, 0)).get
    val target = game.chooseTarget(goblin)
    assert(target.isEmpty)
  }

  test("game.attack") {
    var game = parseGame("""
G....
..G..
..EG.
..G..
...G.""")
    game = game.copy(entities =
      game.entities
        .updated(Pt(1, 2), game.entities.get(Pt(1, 2)).get.copy(hp = 4))
        .updated(Pt(2, 3), game.entities.get(Pt(2, 3)).get.copy(hp = 2))
        .updated(Pt(3, 2), game.entities.get(Pt(3, 2)).get.copy(hp = 2))
    )
    val elf = game.entities.get(Pt(2, 2)).get

    val goblinAbove = game.entities.get(Pt(1, 2)).get
    val after1 = game.attack(elf, goblinAbove)
    var expected1 = parseGame("""
G....
..G..
..EG.
..G..
...G.""")
    assert(after1.toGrid == expected1.toGrid)
    assert(after1.entities.get(Pt(1, 2)).get.hp == 1)

    val goblinRight = after1.entities.get(Pt(2, 3)).get
    val after2 = after1.attack(elf, goblinRight)
    var expected2 = parseGame("""
G....
..G..
..E..
..G..
...G.""")
    assert(after2.toGrid == expected2.toGrid)
    assert(after2.entities.get(Pt(2, 3)).isEmpty)
  }

  test("game.takeTurn") {
    var game = parseGame("""
G....
..G..
..EG.
..G..
...G.""")
    // TODO: this is horrible
    game = game.copy(entities =
      game.entities
        .updated(Pt(1, 2), game.entities.get(Pt(1, 2)).get.copy(hp = 4))
        .updated(Pt(2, 3), game.entities.get(Pt(2, 3)).get.copy(hp = 2))
        .updated(Pt(3, 2), game.entities.get(Pt(3, 2)).get.copy(hp = 2))
    )
    val elf = game.entities.get(Pt(2, 2)).get
    val result = game.takeTurn(elf)
// expect to have killed the weak goblin to the right of the elf
    var expected = game.copy(entities = game.entities.removed(Pt(2, 3)))
    assert(result == expected)
  }

  test("game.playRound") {
    val game = parseGame("""
      #######   
      #.G...#
      #...EG#
      #.#.#G#
      #..G#E#
      #.....#
      #######""")
  }
}
