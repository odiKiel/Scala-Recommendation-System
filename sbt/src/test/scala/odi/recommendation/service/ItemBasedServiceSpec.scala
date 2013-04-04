package odi.recommendation
import org.specs2.mutable._

object ItemBasedServiceSpec extends Specification {
  "ItemBased Algorithm" should {
    TestDatabase.setup()
    val itemBasedServer = ItemBasedService(Services("itemBasedService").toInt)
    val itemBasedClient = new HttpClient("localhost:"+Services("itemBasedService"))
    val ret1 = itemBasedClient.get("/calculateSimilarItems/")
    val ret2 = itemBasedClient.get("/calculateUserPredictions/"+Users.first.get.id.get)

    "should calculate the similarItem values" in {
      ret1.get()
      SimilarItems.all.length mustEqual 10 //Sum_{0-(n-1)} n-i with n=4 
      val items = Items.all
      SimilarItems.getByItemItem(items(2).id.get, items(4).id.get).get.similarity mustEqual 0.7226101.toFloat
    }

    "should not calculate the similarity for the same item" in {
      ret1.get()
      SimilarItems.getByItemItem(Items.first.get.id.get, Items.first.get.id.get) mustEqual None
    }

    "should calculate predictions for all users for the items that they haven't rated yet with the items they already rated" in {
      val prediction = Json.jsonToList(ret2.get()).head
      prediction mustEqual Items.all.last.id.get+"#"+4.075181355016793
    }
  }

}
