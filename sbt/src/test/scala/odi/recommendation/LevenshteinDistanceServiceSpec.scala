package odi.recommendation

import org.specs2.mutable._

object LevenshteinDistanceServiceSpec extends Specification {
  "Levdist with two strings" should {
    LevenshteinDistanceService(Services("levenshteinDistanceService").toInt)
    val levClient = new HttpClient("localhost:"+Services("levenshteinDistanceService"))
    val ret1 = levClient.post("/labelForText/hello", Json.listToJson(List("test", "test2", "helloda", "whup")))
    val ret2 = levClient.post("/labelForText/hello", Json.listToJson(List("test", "test2", "hellodabidu", "whup")))
    "labelForText should return the Label if it has a match" in {
      ret1.get() mustEqual ("hello")
    }
    "labelForText should return an empty String otherwise" in {
      ret2.get() mustEqual ""
    }

    val dfa = fst.toDfa()
    //short explanation: (a,b) a => current letter number, b error count
    "should generate a deterministic final state machine with max distance 3" in {
      dfa.nextState((0,0), 't') mustEqual Set((1,0), (0,1), (1,1), (1,2), (2,2), (2,1), (3,2))
      dfa.nextState((0,0), 'F') mustEqual Set((0,1), (1,2), (1,1), (2,2))
      dfa.nextState((1,0), 'F') mustEqual Set((2,1), (1,1), (2,2), (3,2))
    }
    "returns true if string is in distance (equal)" in {
      dfa.isInDistance("test") mustEqual true
    }
    "returns true if string is in distance (deletion middle)" in {
      dfa.isInDistance("tesst") mustEqual true
    }
    "returns true if string is in distance (deletion beginning)" in {
      dfa.isInDistance("ttest") mustEqual true
    }
    "returns true if string is in distance (deletion end)" in {
      dfa.isInDistance("testt") mustEqual true
    }

    "returns true if string is in distance (substitution beginning)" in {
      dfa.isInDistance("eest") mustEqual true
    }
    "returns true if string is in distance (substitution middle)" in {
      dfa.isInDistance("teat") mustEqual true
    }
    "returns true if string is in distance (substitution end)" in {
      dfa.isInDistance("tesd") mustEqual true
    }

    "returns true if string is in distance (insertion beginning)" in {
      dfa.isInDistance("est") mustEqual true
    }
    "returns true if string is in distance (insertion middle)" in {
      dfa.isInDistance("tst") mustEqual true
    }
    "returns true if string is in distance (insertion end)" in {
      dfa.isInDistance("tes") mustEqual true
    }

    "returns false if string is not in distance (beginning)" in {
      dfa.isInDistance("123test") mustEqual false
    }
    "returns false if string is not in distance (middle)" in {
      dfa.isInDistance("te123st") mustEqual false
    }
    "returns false if string is not in distance (end)" in {
      dfa.isInDistance("test123") mustEqual false
    }

    /*
    "substitude test, rest" in {
      LevenshteinDi("test", "rest") mustEqual 1
    }
    "two things at once test, reste" in {
      Levdist("test", "reste") mustEqual 2
    }
    */
  }
}
