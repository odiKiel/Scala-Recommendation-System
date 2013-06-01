package odi.recommendation
import org.specs2.mutable._

object SVDBasedServiceSpec extends Specification {
  "SVDBased Algorithm" should {
    TestDatabase.setup
    val svdBasedServer = SVDBasedService(Services("svdBasedService").toInt)
    val svdBasedClient = new HttpClient("localhost:"+Services("svdBasedService"))
    val ret1 = svdBasedClient.get("/calculateSimilarUsers/")
    val ret2 = svdBasedClient.get("/calculateUserPredictions/"+Users.first.get.id.get)

    "should calculate the similarUsers values" in {
      ret1.get()
      SimilarUsers.all.length mustEqual 10 //Sum_{0-(n-1)} n-i with n=4 
      val users = Users.all
      SimilarUsers.getByUserUser(users(1).id.get, users(3).id.get).get.similarity mustEqual 0.9974160194396973
    }

    "should not calculate the similarity for the same user" in {
      ret1.get()
      SimilarUsers.getByUserUser(Users.first.get.id.get, Users.first.get.id.get) mustEqual None
    }

    "should calculate predictions for the user, for the items that he hasn't rated yet with the users that are similar" in {
      val prediction = Json.jsonToList(ret2.get())
      prediction mustEqual Items.all.last.id.get+"#"+2.716305083936734
    }

//  "should generate recommendations" in {
//    Ratings.getUnknownItemsForUserByUserWithTag(userId, similarUserId, 25, prefLabels)
//  }
  }

}
