package adventofcode

import org.scalatest.funsuite.AnyFunSuite
import scala.io.Source
import adventofcode.Year2018Day15._
import adventofcode.Utils._

class Year2018Day15Test extends AnyFunSuite {
  def parseGame(s: String): Game =
    Game.fromGrid(Grid.parse(Source.fromString(s)).get)

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
    var m = EntityMap(Map(), Map())
    assert(m.size == 0)

    val e = new ElfEntity(123, Pt(7, 3))
    m = m.add(e)
    assert(m.size == 1)
    assert(m.get(456).isEmpty)
    assert(m.get(123).get == e)
    assert(m.get(Pt(-1, -7)).isEmpty)
    assert(m.get(Pt(7, 3)).get == e)

    m = m.remove(e)
    assert(m.size == 0)
    assert(m.get(123).isEmpty)
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

  test("gameloop.turnOrder") {
    val game = parseGame("#.EG\nG.E#\n")
    val turn = GameLoop.turnOrder(game.entities.entities).map(e => e.pos)
    val expected = Vector(Pt(0, 2), Pt(0, 3), Pt(1, 0), Pt(1, 2))
    assert(turn.sameElements(expected))
  }

  test("gameloop.targets") {
    val game = parseGame("#.EG\nG.E#\n")

    // reverse the order of the entities to test that we sort them properly
    val entities =
      game.entities.entities.toVector
        .sortBy(e => (e.pos.row, e.pos.col))
        .reverse
    val targetsForElf = GameLoop.targets(entities, Elf)
    val targetsForGoblin = GameLoop.targets(entities, Goblin)

    // just filtering doesn't work
    val elves = entities.filter(e => e.typ == Elf)
    val goblins = entities.filter(e => e.typ == Goblin)
    assert(targetsForGoblin != elves)
    assert(targetsForElf != goblins)

    // need them sorted
    assert(targetsForGoblin == elves.sortBy(_.pos.toTuple))
    assert(targetsForElf == goblins.sortBy(_.pos.toTuple))
  }

  test("gameloop.inRange") {
    val game = parseGame("#.EG\nG.E#\n")

    val goblin2 = game.entities.get(Pt(1, 0)).get
    val inRangeOfGoblin2 =
      game.entities.entities.filter(GameLoop.inRange(goblin2))
    assert(inRangeOfGoblin2.isEmpty)

    val elf1 = game.entities.get(Pt(0, 2)).get
    val inRangeOfElf1 = game.entities.entities.filter(GameLoop.inRange(elf1))
    val expected =
      Vector(game.entities.get(Pt(0, 3)).get, game.entities.get(Pt(1, 2)).get)
    assert(inRangeOfElf1 == expected)
  }
}
