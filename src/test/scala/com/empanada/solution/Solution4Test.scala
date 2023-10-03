package com.empanada.solution

import cats.effect.testing.scalatest.AsyncIOSpec
import com.empanada.solution.Solution4.Sections
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec

class Solution4Test extends AsyncWordSpec with Matchers with AsyncIOSpec {

  "fully contains" in {
    val setA = Sections.buildSections("2-8")
    val setB = Sections.buildSections("3-7")

    setA.fullyContains(setB) shouldBe true
    setB.fullyContains(setA) shouldBe false

    val setC = Sections.buildSections("3-9")
    setA.fullyContains(setC) shouldBe false

    val setD = Sections.buildSections("1-4")
    setA.fullyContains(setD) shouldBe false
  }

  "overlaps" in {
    val setA = Sections.buildSections("2-8")
    val setB = Sections.buildSections("8-11")

    setA.overlaps(setB) shouldBe true
    setB.overlaps(setA) shouldBe true

    val setC = Sections.buildSections("3-9")
    setA.overlaps(setC) shouldBe true

    val setD = Sections.buildSections("1-1")
    setA.overlaps(setD) shouldBe false
  }

  "count fully contained example" in {
    val testExample = "./src/test/resources/input-4a.txt"
    Solution4
      .countFullyContainedSections(testExample)
      .asserting(res => res shouldBe 2)
  }

  "count overlaps example" in {
    val testExample = "./src/test/resources/input-4a.txt"
    Solution4
      .countOverlappedSections(testExample)
      .asserting(res => res shouldBe 4)
  }

}