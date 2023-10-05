package com.empanada.solution

import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec

class Solution6Test extends AsyncWordSpec with Matchers with AsyncIOSpec{

  "check repeated" in {
    Solution6.hasRepeatedChars("abbc") shouldBe true
    Solution6.hasRepeatedChars("aebc") shouldBe false
  }

}
