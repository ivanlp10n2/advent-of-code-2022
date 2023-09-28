package com.empanada.solution

import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec

class Solution3Test extends AsyncWordSpec with Matchers with AsyncIOSpec {
  import Solution3._

  "priorities are ordered in alphabet" in {
    import Solution3.Item._
    ('a' to 'z')
      .map(toPriority)
      .foldLeft(0) { (acc, curr) => curr should be > acc; curr }
    ('A' to 'Z')
      .map(toPriority)
      .foldLeft(0) { (acc, curr) => curr should be > acc; curr }

    toPriority('a') shouldBe 1
    toPriority('z') shouldBe 26
    toPriority('A') shouldBe 27
    toPriority('Z') shouldBe 52

  }

  "find duplicates" in {
    val firstCompartment = "LLanbcjio"
    val secondCompartment = "jmvzqwuOI"
    val expected = List('j')
    val rucksack = Rucksack.of(firstCompartment ++ secondCompartment)
    rucksack.duplicatedItems shouldBe expected
  }

  "aggregate duplicated items priorities" in {
    import Solution3.Item._
    val firstCompartment = "LLanmcjio"
    val secondCompartment = "jmvzqwuOI"
    val expectedDuplicates = "jm"
    val rucksack = Rucksack.of(firstCompartment ++ secondCompartment)

    val duplicates = rucksack.duplicatedItems
    expectedDuplicates.foreach { c => assert(duplicates.contains(c)) }
    sumPriorities(duplicates) shouldBe 23
  }

  "example" in {
    val testExample = "./src/test/resources/input-3a.txt"
    Solution3
      .run(testExample)
      .asserting(res => res shouldBe 157)
  }

}
