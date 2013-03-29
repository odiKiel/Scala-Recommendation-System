package odi.recommendation
import org.specs2.mutable._

object ItemBasedServiceSpec extends Specification {
  "ItemBased Algorithm" should {
    TestDatabase.setup()
    val itemBasedServer = ItemBasedService(Services("itemBasedService").toInt)
    val itemBasedClient = new HttpClient("localhost:"+Services("itemBasedService"))
    val ret1 = itemBasedClient.get("/calculateSimilarItems/")
    val ret2 = itemBasedClient.get("/calculatePredictions/"+Users.first.get.id.get)

    "should calculate the similarItem values" in {
      ret1.get()
      SimilarItems.all.length mustEqual 25 
      val items = Items.all
      SimilarItems.getByItemItem(items(2), items(4)).get.similarity mustEqual 0.7226101.toFloat
    }

    "should predict the correct rating for user1 and item5" in {
      //todo
      ret2.get
    }
  }

}
