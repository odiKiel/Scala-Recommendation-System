package odi.recommendation

import org.specs2.mutable._

object LevenshteinDistanceServiceSpec extends Specification {
  "Levdist with two strings" should {
    LevenshteinDistanceService(Services("levenshteinDistanceService").toInt)
    val levClient = new HttpClient("localhost:"+Services("levenshteinDistanceService"))
    val ret1 = levClient.post("/labelForText/hello", Json.listToJson(List("test", "test2", "helloda", "whup")))
    val ret2 = levClient.post("/labelForText/hello", Json.listToJson(List("test", "test2", "hellodabidu", "whup")))
    "labelForText should return the Label if it has a match" in {
      Json.jsonToValue(ret1.get()).head mustEqual ("hello")
    }
    "labelForText should return an empty String otherwise" in {
      Json.jsonToValue(ret2.get()).length mustEqual 0
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
