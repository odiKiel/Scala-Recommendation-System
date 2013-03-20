package odi.recommendation
import org.specs2.mutable._

object TaggerServiceSpec extends Specification {
  "Tagger Service " should {
    TaggerService(Services("taggerService").toInt)
    val tagClient = new HttpClient("localhost:"+Services("taggerService"))
    val ret = tagClient.post("/tagText", Json.listToJson(List("hanf" , "management")))
    //ret onSuccess {response => println("RESPONSE ===> "+response)}
    "tagText should return all STW labels for a text" in {
      //Json.jsonToList(ret.get()) mustEqual List("Hanf", "Management")
      ret.get() mustEqual "test"
    }
  }
}
