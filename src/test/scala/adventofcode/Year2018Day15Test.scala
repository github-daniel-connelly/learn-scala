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
    val turn = game.turnOrder.map(game.entities.pos)
    val expected = Vector(Pt(0, 2), Pt(0, 3), Pt(1, 0), Pt(1, 2))
    assert(turn == expected)
  }

  test("game.targets") {
    val game = parseGame("#.EG\nG.E#\n")
    val elf2 = game.entities.find(Pt(1, 2)).get
    val targetsForElf = game.targets(elf2)
    val goblin1 = game.entities.find(Pt(0, 3)).get
    val targetsForGoblin = game.targets(goblin1)
    val elves = game.entities.iterator.filter(e => e.typ == Elf)
    val goblins = game.entities.iterator.filter(e => e.typ == Goblin)
    assert(targetsForGoblin == elves.toVector.sortBy(_.pos.toTuple))
    assert(targetsForElf == goblins.toVector.sortBy(_.pos.toTuple))
  }

  test("game.inRange") {
    val game = parseGame("#.EG\nG.E#\n")

    val goblin2 = game.entities.find(Pt(1, 0)).get
    val inRangeOfGoblin2 =
      game.entities.iterator.filter(e => game.inRange(goblin2, e)).toVector
    assert(inRangeOfGoblin2.isEmpty)

    val elf1 = game.entities.find(Pt(0, 2)).get
    val inRangeOfElf1 =
      game.entities.iterator.filter(e => game.inRange(elf1, e)).toVector
    val expected =
      Vector(game.entities.find(Pt(0, 3)).get, game.entities.find(Pt(1, 2)).get)
    assert(inRangeOfElf1 == expected)
  }

  test("game.candidateDestinations") {
    val game = parseGame("#..E\nG.E#\n")
    val goblin = game.entities.find(Pt(1, 0)).get
    val dests = game.candidateDestinations(game.targets(goblin))
    val expected = Vector(Pt(0, 2), Pt(1, 1))
    assert(dests == expected)
  }

  test("game.dists") {
    val game = parseGame("""
      #######
      #..G..#
      #....G#
      #.#G#G#
      #...#E#
      #.....#
      #######""")
    val goblin = game.entities.find(Pt(2, 5)).get
    val elf = game.entities.find(Pt(4, 5)).get
    assert(
      game.dists(goblin.pos, Vector(Pt(5, 5)))(Pt(5, 5)) == 11
    )
  }

  test("game.destination") {
    val game = parseGame("#######\n#E..G.#\n#...#.#\n#.G.#G#\n#######\n")
    val elf = game.entities.find(Pt(1, 1)).get

    val targets = game.targets(elf)
    val expectedTargets =
      Seq(Pt(1, 4), Pt(3, 2), Pt(3, 5)).map(p => game.entities.find(p).get)
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

    val Dest(pos, dist) = game.destination(elf).asInstanceOf[Dest]
    assert(pos == Pt(1, 3))
    assert(dist == 2)
  }

  test("game.destination.notargets") {
    val game = parseGame("##E#\nE.##\n")
    val elf1 = game.entities.find(Pt(0, 2)).get
    assert(game.destination(elf1) == NoTargets)
  }

  test("game.destination.none") {
    val game = parseGame("##E#\nG.##\n")
    val elf1 = game.entities.find(Pt(0, 2)).get
    assert(game.destination(elf1) == NoneReachable)
  }

  test("game.paths") {
    val game = parseGame("""
      #######
      #.E...#
      #.....#
      #...G.#
      #######
      """)
    val elf = game.entities.find(Pt(1, 2)).get
    val Dest(dst, dist) = game.destination(elf).asInstanceOf[Dest]
    assert(dst == Pt(2, 4))
    assert(dist == 3)
    val paths = game.paths(elf.pos, dst, dist)
    val expected = Set(
      List(Pt(2, 2), Pt(2, 3), Pt(2, 4)),
      List(Pt(1, 3), Pt(2, 3), Pt(2, 4)),
      List(Pt(1, 3), Pt(1, 4), Pt(2, 4))
    )
    assert(paths.toSet == expected)
    assert(game.step(elf.pos, dst, dist) == Pt(1, 3))
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
        .update(start.entities.find(Pt(1, 2)).get.copy(hp = 4))
        .update(start.entities.find(Pt(2, 3)).get.copy(hp = 2))
        .update(start.entities.find(Pt(3, 2)).get.copy(hp = 2))
    )
    val elf = game.entities.find(Pt(2, 2)).get
    val target = game.chooseTarget(elf).get
    assert(target == game.entities.find(Pt(2, 3)).get)
  }

  test("game.chooseTarget.none") {
    val game = parseGame("""
      G....
      ..G..
      ..EG.
      ..G..
      ...G.""")
    val goblin = game.entities.find(Pt(0, 0)).get
    val target = game.chooseTarget(goblin)
    assert(target.isEmpty)
  }

  test("game.move") {
    var game = parseGame("""
      #######
      #...G.#
      #..G.G#
      #.#.#G#
      #...#E#
      #.....#
      #######""")
    val elf = game.entities.find(Pt(4, 5)).get
    val goblin1 = game.entities.find(Pt(1, 4)).get
    val goblin2 = game.entities.find(Pt(2, 3)).get
    val goblin3 = game.entities.find(Pt(2, 5)).get
    val goblin4 = game.entities.find(Pt(3, 5)).get
    val result = game
      .move(goblin1.id)
      .get
      .move(goblin2.id)
      .get
      .move(goblin3.id)
      .get
      .move(goblin4.id)
      .get
    var expected = parseGame("""
      #######
      #..G..#
      #...G.#
      #.#G#G#
      #...#E#
      #.....#
      #######""")
    assert(result.toGrid == expected.toGrid)
  }

  test("game.attack") {
    var start = parseGame("""
      G....
      G.G..
      .E.G.
      .....
      ...G.""")

    val elf = start.entities.find(Pt(2, 1)).get
    val goblin1 = start.entities.find(Pt(1, 0)).get.copy(hp = 4)
    val goblin2 = start.entities.find(Pt(1, 2)).get.copy(hp = 2)
    val goblin3 = start.entities.find(Pt(2, 3)).get.copy(hp = 2)
    val game = start.copy(entities =
      start.entities.update(goblin1).update(goblin2).update(goblin3)
    )

    // expect to have moved up and killed the weak goblin to the right
    var expected = game.copy(entities =
      game.entities.step(elf.id, Pt(1, 1)).remove(goblin2.id)
    )
    val result = game.takeTurn(elf.id).get
    assert(result == expected)
  }

  test("game.takeTurn") {
    var start = parseGame("""
G....
G.G..
.E.G.
.....
...G.""")

    val elf = start.entities.find(Pt(2, 1)).get
    val goblin1 = start.entities.find(Pt(1, 0)).get.copy(hp = 4)
    val goblin2 = start.entities.find(Pt(1, 2)).get.copy(hp = 2)
    val goblin3 = start.entities.find(Pt(2, 3)).get.copy(hp = 2)
    val game = start.copy(entities =
      start.entities.update(goblin1).update(goblin2).update(goblin3)
    )

    // expect to have moved up and killed the weak goblin to the right
    var expected = game.copy(entities =
      game.entities.step(elf.id, Pt(1, 1)).remove(goblin2.id)
    )

    val result = game.takeTurn(elf.id).get

    assert(result.toGrid == expected.toGrid)
    assert(result == expected)
  }

  test("game.playRound") {
    assert(parseGame("""
      #######   
      #.G...#
      #...EG#
      #.#.#G#
      #..G#E#
      #.....#
      #######
      """).result == 27730)
    assert(parseGame("""
      #######
      #G..#E#
      #E#E.E#
      #G.##.#
      #...#E#
      #...E.#
      #######
      """).result == 36334)
    assert(parseGame("""
      #######
      #.E...#
      #.#..G#
      #.###.#
      #E#G#G#
      #...#G#
      #######
      """).result == 28944)
    assert(parseGame("""
      #########
      #G......#
      #.E.#...#
      #..##..G#
      #...##..#
      #...#...#
      #.G...G.#
      #.....G.#
      #########
      """).result == 18740)
    assert(parseGame("""
      #######
      #E.G#.#
      #.#G..#
      #G.#.G#
      #G..#.#
      #...E.#
      #######
      """).result == 27755)
    assert(parseGame("""
      #######
      #E..EG#
      #.#G.E#
      #E.##E#
      #G..#.#
      #..E#.#
      #######
      """).result == 39514)
  }
}
