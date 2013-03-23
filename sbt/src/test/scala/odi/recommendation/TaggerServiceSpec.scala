package odi.recommendation
import org.specs2.mutable._

object TaggerServiceSpec extends Specification {
  "Tagger Service " should {
    TaggerService(Services("taggerService").toInt)
    val tagClient = new HttpClient("localhost:"+Services("taggerService"))
    val ret = tagClient.post("/tagText", "This is a text about marketingplanung and statistics have fun with it.")
    //ret onSuccess {response => println("RESPONSE ===> "+response)}
    "tagText should return all STW labels for a text" in {
      val returnList = Json.jsonToList(ret.get())
      returnList.contains("Marketingplanung") mustEqual true
      returnList.contains("Statistics") mustEqual true
    }

    val dfsmList = TaggerService.createDfsm(List("this", "is", "a", "test"))
    "create a list of deterministic finite state machines for a text" in {
      dfsmList.size mustEqual(4)
    }

    "labelForText with word in list" in {
      TaggerService.labelForText("this", dfsmList) mustEqual true
    }

    "labelForText with word not in list" in {
      TaggerService.labelForText("notIncluded", dfsmList) mustEqual false
    }

    "labelForText with a word that has two errors" in {
      TaggerService.labelForText("thisbb", dfsmList) mustEqual true
    }

  }
}
