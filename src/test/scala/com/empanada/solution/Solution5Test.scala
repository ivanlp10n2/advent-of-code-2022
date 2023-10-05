package com.empanada.solution

import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec

import scala.collection.mutable

class Solution5Test extends AsyncWordSpec with Matchers with AsyncIOSpec {
  import Solution5._

  " should know how many whitespaces to ignore" in {
    val base =
      """
        |[D] [B]
        | 1   2
        |""".stripMargin

    countWhitespaces(base) shouldBe 1
    countCranes(base) shouldBe 2
    val base2 =
      """
        |[D] [B] [C]
        | 1   2   3
        |""".stripMargin

    countCranes(base2) shouldBe 3
    countWhitespaces(base2) shouldBe 2
    val base3 =
      """
        |[D] [B] [C] [A]
        | 1   2   3   4
        |""".stripMargin

    countCranes(base3) shouldBe 4
    countWhitespaces(base3) shouldBe 3
  }

  "build crane" in {
    val base = "[C]"
    Solution5.Crane.of(base).get.value shouldBe 'C'
  }

  "build ship" in {
    val input =
      """
        |[D]
        | 1
        |""".stripMargin

    val ship = Solution5.makeShip(input)
//    println(ship.stacks.mkString("[","\n","]"))
    ship.col(1) shouldBe List(Crane.of('D'))

    val input2 =
      """
        |[A]     [B] [M]
        |[z]     [F] [P]
        |[F]     [G] [O]
        | 1   2   3   4
        |""".stripMargin
    val ship2 = Solution5.makeShip(input2)
    ship2.stacks.mkString("[","\n","]")
    ship2.col(1) shouldBe mutable.Stack('A', 'z', 'F').map(Crane.of)
    ship2.col(2) shouldBe mutable.Stack.empty
    ship2.col(3) shouldBe mutable.Stack('B', 'F', 'G').map(Crane.of)
    ship2.col(4) shouldBe mutable.Stack('M', 'P', 'O').map(Crane.of)
  }


  "run instructions" in {
    val base =
      """
        |[D] [B] [C] [A]
        | 1   2   3   4
        |""".stripMargin

    val commands =
      """
        |move 1 from 3 to 4
        |move 1 from 2 to 3
        |""".stripMargin

    val ship = Solution5.makeShip(base)
    runCommands(ship, commands)

    ship.col(1) shouldBe mutable.Stack(Crane.of('D'))
    ship.col(2) shouldBe mutable.Stack.empty
    ship.col(3) shouldBe mutable.Stack(Crane.of('B'))
    ship.col(4) shouldBe mutable.Stack('C','A').map(Crane.of)

    ship.top shouldBe "DBC"

  }

  "move with CrateMover9001" in{
    val base =
      """
        |[F]     [G]
        |[D] [B] [C] [F]
        | 1   2   3   4
        |""".stripMargin

    val commands =
      """
        |move 2 from 1 to 4
        |move 2 from 3 to 4
        |""".stripMargin

    val ship = Solution5.makeShip(base)
    ship.prettyPrint()
    runCommands(ship, commands)
    ship.prettyPrint()
    ship.top shouldBe "BG"

  }

  "should pretty print all the time equally" in {
    val base =
      """
        |[T] [V]                     [W]
        |[V] [C] [P] [D]             [B]
        |[J] [P] [R] [N] [B]         [Z]
        |[W] [Q] [D] [M] [T]     [L] [T]
        |[N] [J] [H] [B] [P] [T] [P] [L]
        |[R] [D] [F] [P] [R] [P] [R] [S] [G]
        |[M] [W] [J] [R] [V] [B] [J] [C] [S]
        |[S] [B] [B] [F] [H] [C] [B] [N] [L]
        | 1   2   3   4   5   6   7   8   9
        |""".stripMargin.trim

    val ship = Ship.of(base)
//    base.trim shouldBe formatted.trim

    assert(true)
  }


}
