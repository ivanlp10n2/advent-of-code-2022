package com.empanada.solution

import cats.effect.testing.scalatest.AsyncIOSpec
import com.empanada.solution.Solution4.Sections
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec

class Solution4Test extends AsyncWordSpec with Matchers with AsyncIOSpec {

  "fully contains" in {
    val setA = Sections.buildSections("2-8")
    val setB = Sections.buildSections("3-7")

    setA.contains(setB) shouldBe true
    setB.contains(setA) shouldBe false

    val setC = Sections.buildSections("3-9")
    setA.contains(setC) shouldBe false

    val setD = Sections.buildSections("1-4")
    setA.contains(setD) shouldBe false
  }

  "example" in {
    val testExample = "./src/test/resources/input-4a.txt"
    Solution4
      .run(testExample)
      .asserting(res => res shouldBe 2)
  }

}