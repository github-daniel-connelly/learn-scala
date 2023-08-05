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

  def moving(entity: Entity, game: Game): MovingEntity =
    new MovingEntity(entity, game.entities, game.nbrs)

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

    val expectedEntities = Vector(
      (Pt(0, 2), Elf, 200, 3),
      (Pt(0, 3), Goblin, 200, 3),
      (Pt(1, 0), Goblin, 200, 3),
      (Pt(1, 2), Elf, 200, 3)
    )
    val entities = game.entities.toVector.sortBy(_.pos.toTuple)
    assert(entities.map(e => (e.pos, e.typ, e.hp, e.ap)) == expectedEntities)

    val expectedMap = Map(
      Pt(0, 0) -> Wall,
      Pt(0, 1) -> Floor,
      Pt(0, 2) -> Floor,
      Pt(0, 3) -> Floor,
      Pt(1, 0) -> Floor,
      Pt(1, 1) -> Floor,
      Pt(1, 2) -> Floor,
      Pt(1, 3) -> Wall
    )
    assert(game.map == expectedMap)
  }

  test("entity.turnOrder") {
    val game = parseGame("""
      #######
      #...G.#
      #..G.G#
      #.#.#G#
      #...#E#
      #.....#
      #######""")
    val dest = Pt(5, 5)
    val turn = game.turnOrder.map(game.entities.get).map(_.pos)
    val expected = Vector(Pt(1, 4), Pt(2, 3), Pt(2, 5), Pt(3, 5), Pt(4, 5))
    assert(turn == expected)
  }

  test("entity.targets") {
    val game = parseGame("#.EG\nG.E#\n")
    val elf2 = moving(game.entities.find(Pt(1, 2)).get, game)
    val goblin1 = moving(game.entities.find(Pt(0, 3)).get, game)
    val elves = game.entities.iterator.filter(e => e.typ == Elf)
    val goblins = game.entities.iterator.filter(e => e.typ == Goblin)
    assert(goblin1.targets == elves.toVector.sortBy(_.pos.toTuple))
    assert(elf2.targets == goblins.toVector.sortBy(_.pos.toTuple))
  }

  test("entity.candidateDestinations") {
    val game = parseGame("""
      #######
      #...G.#
      #..G.G#
      #.#.#G#
      #...#E#
      #.....#
      #######""")
    val goblin1 = moving(game.entities.find(Pt(1, 4)).get, game)
    val goblin2 = moving(game.entities.find(Pt(1, 4)).get, game)
    val goblin3 = moving(game.entities.find(Pt(1, 4)).get, game)
    val expected = Vector(Pt(5, 5))
    for (goblin <- Seq(goblin1, goblin2, goblin3)) {
      val dests = goblin.candidateDestinations(goblin.targets)
      assert(dests == expected)
    }
  }

  test("entity.dists") {
    val game = parseGame("""
      #######
      #..G..#
      #....G#
      #.#G#G#
      #...#E#
      #.....#
      #######""")
    val dest = Pt(5, 5)
    val goblin1 = moving(game.entities.find(Pt(1, 3)).get, game)
    assert(Search.dists(goblin1.pos, Vector(dest), game.nbrs)(dest) == 10)
    val goblin2 = moving(game.entities.find(Pt(3, 3)).get, game)
    assert(Search.dists(goblin2.pos, Vector(dest), game.nbrs)(dest) == 4)
    val goblin3 = moving(game.entities.find(Pt(2, 5)).get, game)
    assert(Search.dists(goblin3.pos, Vector(dest), game.nbrs)(dest) == 11)
  }

  test("entity.destination") {
    val game = parseGame("""
      #######
      #...G.#
      #..G.G#
      #.#.#G#
      #...#E#
      #.....#
      #######""")
    val goblin1 = moving(game.entities.find(Pt(1, 4)).get, game)
    val goblin2 = moving(game.entities.find(Pt(1, 4)).get, game)
    val goblin3 = moving(game.entities.find(Pt(1, 4)).get, game)
    for (goblin <- Seq(goblin1, goblin2, goblin3)) {
      val dest = goblin.destination
      assert(dest.asInstanceOf[Dest].pos == Pt(5, 5))
    }
  }

  test("game.destination") {
    val game = parseGame("#######\n#E..G.#\n#...#.#\n#.G.#G#\n#######\n")
    val elf = moving(game.entities.find(Pt(1, 1)).get, game)

    val expectedTargets =
      Seq(Pt(1, 4), Pt(3, 2), Pt(3, 5)).map(p => game.entities.find(p).get)
    assert(elf.targets == expectedTargets)

    val destinations = elf.candidateDestinations(elf.targets)
    val expectedDestinations = Seq(
      Pt(1, 3),
      Pt(1, 5),
      Pt(2, 2),
      Pt(2, 5),
      Pt(3, 1),
      Pt(3, 3)
    )
    assert(destinations == expectedDestinations)

    val dists = Search.dists(elf.pos, destinations.toSet, game.nbrs)
    val expectedDists = Map(
      (Pt(1, 3), 2),
      (Pt(2, 2), 2),
      (Pt(3, 1), 2),
      (Pt(3, 3), 4)
    )
    assert(dists == expectedDists)

    val Dest(pos, dist) = elf.destination.asInstanceOf[Dest]
    assert(pos == Pt(1, 3))
    assert(dist == 2)
  }

  test("game.destination.notargets") {
    val game = parseGame("##E#\nE.##\n")
    val elf1 = moving(game.entities.find(Pt(0, 2)).get, game)
    assert(elf1.destination == NoTargets)
  }

  test("game.destination.none") {
    val game = parseGame("##E#\nG.##\n")
    val elf1 = moving(game.entities.find(Pt(0, 2)).get, game)
    assert(elf1.destination == NoneReachable)
  }

  test("entity.step") {
    val game = parseGame("""
      #######
      #.E...#
      #.....#
      #...G.#
      #######
      """)
    val elf = moving(game.entities.find(Pt(1, 2)).get, game)
    val Dest(dst, dist) = elf.destination.asInstanceOf[Dest]
    assert(dst == Pt(2, 4))
    assert(dist == 3)
    assert(elf.step(dst, dist) == Pt(1, 3))
  }

  test("entity.move") {
    var game = parseGame("""
      #######
      #...G.#
      #..G.G#
      #.#.#G#
      #...#E#
      #.....#
      #######""")

    def move(id: Id, entities: Entities): Result[Entities] =
      entities
        .get(id)
        .move(entities, game.copy(entities = entities).nbrs)

    val elf = game.entities.find(Pt(4, 5)).get
    val goblin1 = game.entities.find(Pt(1, 4)).get
    val goblin2 = game.entities.find(Pt(2, 3)).get
    val goblin3 = game.entities.find(Pt(2, 5)).get
    val goblin4 = game.entities.find(Pt(3, 5)).get
    val entities = game.entities
    val result = (for {
      entities <- move(goblin1.id, entities)
      entities <- move(goblin2.id, entities)
      entities <- move(goblin3.id, entities)
      entities <- move(goblin4.id, entities)
    } yield entities).value
    val resultPairs = result.toVector.map(e => (e.pos, e.typ))

    var expected = parseGame("""
      #######
      #..G..#
      #...G.#
      #.#G#G#
      #...#E#
      #.....#
      #######""")
    val expectedPairs = expected.entities.toVector.map(e => (e.pos, e.typ))
    assert(resultPairs == expectedPairs)
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
    val result = game.copy(entities =
      Entity.takeTurn(elf.id, game.entities, game.nbrs).value
    )
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

    val result = game.copy(entities =
      Entity.takeTurn(elf.id, game.entities, game.nbrs).value
    )

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
      """).play == 27730)
    assert(parseGame("""
      #######
      #G..#E#
      #E#E.E#
      #G.##.#
      #...#E#
      #...E.#
      #######
      """).play == 36334)
    assert(parseGame("""
      #######
      #.E...#
      #.#..G#
      #.###.#
      #E#G#G#
      #...#G#
      #######
      """).play == 28944)
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
      """).play == 18740)
    assert(parseGame("""
      #######
      #E.G#.#
      #.#G..#
      #G.#.G#
      #G..#.#
      #...E.#
      #######
      """).play == 27755)
    assert(parseGame("""
      #######
      #E..EG#
      #.#G.E#
      #E.##E#
      #G..#.#
      #..E#.#
      #######
      """).play == 39514)
  }
}
