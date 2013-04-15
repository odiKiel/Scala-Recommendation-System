package odi.recommendation
import org.jboss.netty.handler.codec.http.{HttpResponse}
import com.twitter.finagle.builder.Server
import com.twitter.util.{Promise, Future}

object ItemBasedService extends HttpServer with ListOperation {
  val name = "ItemBasedService"

  def apply(port: Int): Int = {
    super.apply(port, name)
  }

  def callPostMethod(path: Array[String], value: String): Future[HttpResponse] = {
    path.head match {
      case _ => Future.value(createHttpResponse("No such method"))
    }
  }

  def callGetMethod(path: Array[String]): Future[HttpResponse] = {
    path.head match {
      case "calculateSimilarItems" => getCalculateSimilarItems(path)
      case "calculateUserPredictions" => getCalculateUserPredictions(path.tail.head.toInt, path.tail)
      case _ => Future.value(createHttpResponse("No such method ItemBasedService"))
    }
  }

  //for each item that is connectet with a user find all items from the user and calculate the similarity with the original item
  //should work with a futurepool
  //create itemIdUserId hash as itemId -> (Rating, Rating) even better create two vectors itemVector++currentUserRating, itemUserIdVector++curentUserRatnig
  def getCalculateSimilarItems(path: Array[String]): Future[HttpResponse] = {
    val items: List[Int] = Items.allIds
    val purchasedTogether = collection.mutable.Set[(Int, Int)]() //all items that where purchased together by one or more users

    // log for testing
    var i = 1
    for(itemId: Int <- items) {
      println("working on item: "+i)
      i+=1
      val itemIdUserIdHash = collection.mutable.HashMap[Int, List[Int]]()
      val localPurchasedTogether = collection.mutable.Set[(Int, Int)]() //all items that where purchased together with the current item use this for easy creation of purchasedTogether
      for(userId: Int <- Users.userIdsForItemId(itemId);
          itemUserId: Int <- Items.allItemIdsUserId(userId))
      {
        if(itemId != itemUserId && !(purchasedTogether.contains((itemId, itemUserId)) || purchasedTogether.contains((itemUserId, itemId)))) {
          localPurchasedTogether += ((itemId, itemUserId))
          itemIdUserIdHash += itemUserId -> addToList[Int](itemIdUserIdHash.get(itemUserId), userId)
        }
      }

      purchasedTogether ++= localPurchasedTogether
      SimilarItems.calculateSimilarity(itemId, itemIdUserIdHash) 
    }

    Future.value(createHttpResponse("done"))
  }



  // userItems -> SimilarItems - userItems -> calculate predictions
  //todo improve this with k-nearest
  def getCalculateUserPredictions(userId: Int, path: Array[String]): Future[HttpResponse] = {

    //save the items that are unknown together with the item that are known and their similarity values
    val similarItems = collection.mutable.HashMap[Int, List[(Int, Double)]]()
    val allItemsUser = Items.allItemIdsUserId(userId)

    for(userItemId: Int <- allItemsUser;
        (similarItemId, similarity) <- SimilarItems.byItemId(userItemId)) 
    {
      if(!allItemsUser.contains(similarItemId) && similarity > 0) {//item is unknown to the user and similarity is not independence
        similarItems += similarItemId -> addToList[(Int, Double)](similarItems.get(similarItemId), (userItemId, similarity))

      }
    }

    val recommendations = similarItems.flatMap({case (itemId: Int, similarItemList: List[(Int, Double)]) => Map(itemId.toString -> calculatePrediction(userId, similarItemList).toString)})

    Future.value(createHttpResponse(Json.toJson(recommendations)))
  }

  //calculate the prediction for one item from one User by the items that he already rated
  //integrate the average rating
  def calculatePrediction(userId: Int, similarItems: List[(Int, Double)]): Double = {
    similarItems.length match {
      case 0 => 0
      case _ => {
        val numerator = similarItems.map({case (itemId, similarity) => {
          Ratings.byItemIdUserId(itemId, userId).get.rating * similarity
        }}).sum 
        (numerator / similarItems.map(_._2).sum)
      }
    }
  }

}
