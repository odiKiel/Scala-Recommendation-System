package odi.recommendation
import org.specs2.mutable._

object RecommendationServiceSpec extends Specification {
  "Recommendation Service" should {
    TestDatabase.setup()
    val recommendationServer = RecommendationService(Services("recommendationService").toInt)
    val recommendationClient = new HttpClient("localhost:"+Services("recommendationService"))
    val ret1 = recommendationClient.get("/calculateSimilarities/")
    val ret2 = recommendationClient.get("/calculateUserPredictions/"+Users.first.get.id.get)

    "should calculate the similarItems and similarUsers values" in {
      ret1.get()
      SimilarItems.all.length mustEqual 10 //Sum_{0-(n-1)} n-i with n=4 
      SimilarUsers.all.length mustEqual 10
      val items = Items.all
      SimilarItems.getByItemItem(items(2).id.get, items(4).id.get).get.similarity mustEqual 0.7226101.toFloat
      val users = Users.all
      SimilarUsers.getByUserUser(users(1).id.get, users(3).id.get).get.similarity mustEqual 0.9974160194396973
    }

    "should not calculate the similarities for the same item and user" in {
      ret1.get()
      SimilarItems.getByItemItem(Items.first.get.id.get, Items.first.get.id.get) mustEqual None
      SimilarUsers.getByUserUser(Users.first.get.id.get, Users.first.get.id.get) mustEqual None
    }

    "should calculate predictions for all users for the items that they haven't rated yet with the items they already rated" in {
      val prediction = Json.jsonToList(ret2.get())
      prediction.head mustEqual Items.all.last.id.get+"#"+2.716305083936734
    }
  }

}
