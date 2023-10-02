package com.empanada.solution

import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec

class Solution3FPTest extends AsyncWordSpec with Matchers with AsyncIOSpec {
  import Solution3FP._

  "priorities are ordered in alphabet" in {
    import Solution3FP.Item._
    toPriority('a') shouldBe 1
    ('a' to 'z')
      .map(toPriority)
      .foldLeft(0) { (acc, curr) => curr should be > acc; curr }
    toPriority('z') shouldBe 26

    toPriority('A') shouldBe 27
    ('A' to 'Z')
      .map(toPriority)
      .foldLeft(0) { (acc, curr) => curr should be > acc; curr }
    toPriority('Z') shouldBe 52
  }

  "find duplicates" in {
    val firstCompartment = "LLanbcjio"
    val secondCompartment = "jmvzqwuOI"
    val expected = List('j')
    val rucksack = Rucksack.of(firstCompartment ++ secondCompartment)
    rucksack.duplicates shouldBe expected
  }

  "aggregate duplicated items priorities" in {
    import Solution3FP.Item._
    val firstCompartment = "LLanmcjio"
    val secondCompartment = "jmvzqwuOI"
    val expectedDuplicates = "jm"
    val rucksack = Rucksack.of(firstCompartment ++ secondCompartment)

    rucksack.duplicates.foreach { c => assert(expectedDuplicates.contains(c)) }
    expectedDuplicates.foreach { c => assert(rucksack.duplicates.contains(c)) }
    sumPriorities(rucksack.duplicates) shouldBe 23
  }

  "example" in {
    val testExample = "./src/test/resources/input-3a.txt"
    Solution3FP
      .runPriorityTypesSum(testExample)
      .asserting(res => res shouldBe 157)
  }

  "count badges" in {
    val testExample = "./src/test/resources/input-3a.txt"
    Solution3FP
      .runPriorityBadgeSum(testExample)
      .asserting(res => res shouldBe 70)
  }

}
