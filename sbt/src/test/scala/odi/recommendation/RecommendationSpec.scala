package odi.recommendation

import org.specs2.mutable._

object levdistSpec extends Specification {
  "Levdist with two strings" should {
    "reduction test, test1" in {
      Levdist("test", "test1") mustEqual 1
    }
    "add element test1, test" in {
      Levdist("test1", "test") mustEqual 1
    }
    "substitude test, rest" in {
      Levdist("test", "rest") mustEqual 1
    }
    "two things at once test, reste" in {
      Levdist("test", "reste") mustEqual 2
    }
  }
}
