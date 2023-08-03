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
    val elves = game.entities.toVector.filter(e => e.typ == Elf)
    val goblins = game.entities.toVector.filter(e => e.typ == Goblin)
    assert(targetsForGoblin == elves.sortBy(_.pos.toTuple))
    assert(targetsForElf == goblins.sortBy(_.pos.toTuple))
  }

  test("game.inRange") {
    val game = parseGame("#.EG\nG.E#\n")

    val goblin2 = game.entities.get(Pt(1, 0)).get
    val inRangeOfGoblin2 = game.entities.toVector.filter(goblin2.inRange)
    assert(inRangeOfGoblin2.isEmpty)

    val elf1 = game.entities.get(Pt(0, 2)).get
    val inRangeOfElf1 = game.entities.toVector.filter(elf1.inRange)
    val expected =
      Vector(game.entities.get(Pt(0, 3)).get, game.entities.get(Pt(1, 2)).get)
    assert(inRangeOfElf1 == expected)
  }

  test("game.targetsInRange") {
    val game = parseGame("#.EG\nG.E#\n")

    val goblin2 = game.entities.get(Pt(1, 0)).get
    assert(game.targetsInRange(goblin2).isEmpty)

    val elf1 = game.entities.get(Pt(0, 2)).get
    val expected = Vector(game.entities.get(Pt(0, 3)).get)
    assert(game.targetsInRange(elf1) == expected)
  }

  test("game.destinations") {
    val game = parseGame("#..E\nG.E#\n")
    val goblin = game.entities.get(Pt(1, 0)).get
    val dests = game.destinations(goblin)
    val expected = Vector(Pt(0, 2), Pt(1, 1))
    assert(dests == expected)
  }
}
