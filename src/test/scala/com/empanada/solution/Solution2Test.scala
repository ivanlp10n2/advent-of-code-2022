package com.empanada.solution

import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec

class Solution2Test extends AsyncWordSpec with Matchers with AsyncIOSpec{
  import Solution2.Shape._
  import Solution2.Outcome._

  "simple selection" in {
    val testExample = "./src/test/resources/input-2a.txt"
    Solution2.run(testExample)
      .asserting(res => res shouldBe 12)
  }

  "rock and paper" in {
    val rock = Rock
    val paper = Paper

    rock.vs(paper) shouldBe Defeat
    paper.vs(rock) shouldBe Victory
  }

  "paper and scissors" in {
    val paper = Paper
    val scissors = Scissors

    paper.vs(scissors) shouldBe Defeat
    scissors.vs(paper) shouldBe Victory
  }

  "scissors and rock" in {
    val scissors = Scissors
    val rock = Rock

    scissors.vs(rock) shouldBe Defeat
    rock.vs(scissors) shouldBe Victory
  }

  "draw" in {
    val rock = Rock
    val paper = Paper
    val scissors = Scissors

    rock.vs(rock) shouldBe Draw
    paper.vs(paper) shouldBe Draw
    scissors.vs(scissors) shouldBe Draw
  }

}
