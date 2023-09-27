package com.empanada.solution

import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec

class Solution1Test extends AsyncWordSpec with Matchers with AsyncIOSpec{

  "simple selection" in {
    val testExample = "./src/test/resources/input-1a.txt"
    Solution1.run(testExample)
      .asserting(res => res shouldBe 123)
  }

  "aggregated selection" in {
    val testExample = "./src/test/resources/input-1b.txt"
    Solution1.run(testExample)
      .asserting(res => res shouldBe 6)
  }

}
