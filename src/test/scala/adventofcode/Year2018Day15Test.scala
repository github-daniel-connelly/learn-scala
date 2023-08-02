package adventofcode

import org.scalatest.funsuite.AnyFunSuite
import scala.io.Source
import adventofcode.Year2018Day15._

class Year2018Day15Test extends AnyFunSuite {
  test("parsing") {
    val src = Source.fromString("#.EG\nG.E#\n")
    val grid = parse(src)
    assert(grid.isSuccess)

    val expected = Grid(
      Array(
        Array(Wall, Floor, Elf, Goblin),
        Array(Goblin, Floor, Elf, Wall)
      )
    )
    assert(grid.get == expected)
  }

  test("parsing.failure") {
    val src = Source.fromString("foo")
    val grid = Year2018Day15.parse(src)
    assert(grid.isFailure)
  }
}
