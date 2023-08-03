package adventofcode

import org.scalatest.funsuite.AnyFunSuite
import scala.io.Source
import adventofcode.Year2018Day15._
import adventofcode.Utils._

class Year2018Day15Test extends AnyFunSuite {
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
}
