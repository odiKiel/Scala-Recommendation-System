package odi.recommendation
import org.jboss.netty.handler.codec.http.{HttpResponse}
import com.twitter.finagle.builder.Server
import com.twitter.util.{Promise, Future}
import org.apache.commons.math3.linear._


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
    if(path.size < 1) Future.value(createHttpResponse("Not enough parameter for ItemBasedService"))
    else {
      path.head match {
        case "calculateSimilarItems" => getCalculateSimilarItems(path)
        case "calculateUserPrediction" => {
          if(path.size > 1) getCalculateUserPrediction(path.tail.head.toInt, path.tail.tail)
          else Future.value(createHttpResponse("Not enough parameters"))
        }
        case _ => Future.value(createHttpResponse("No such method ItemBasedService"))
      }
    }
  }

  //for each item that is connectet with a user find all items from the user and calculate the similarity with the original item
  //should work with a futurepool
  //todo delete all items on each run?
  def getCalculateSimilarItems(path: Array[String]): Future[HttpResponse] = {
    val items: List[Int] = Items.allIds
    SimilarItems.deleteAll
    val purchasedTogether = collection.mutable.Set[(Int, Int)]() //all items that where purchased together by one or more users

    // log for testing
    for(itemId: Int <- items) {
      //val itemIdUserIdHash = collection.mutable.HashMap[Int, List[Int]]()
      val itemMapRatingsVector = collection.mutable.HashMap[Int, (RealVector, RealVector)]()
      val localPurchasedTogether = collection.mutable.Set[(Int, Int)]() //all items that where purchased together with the current item use this for easy creation of purchasedTogether
      for((userId: Int, userRating: Double) <- Users.userIdsForItemIdWithRatingNormalized(itemId);
          (itemUserId: Int, itemUserRating: Double) <- Items.allItemIdsForUserIdWithRatingNormalized(userId))
      {
        if(itemId != itemUserId && !(purchasedTogether.contains((itemId, itemUserId)) || purchasedTogether.contains((itemUserId, itemId)))) {
          localPurchasedTogether += ((itemId, itemUserId))
          itemMapRatingsVector += itemUserId -> addToVector(itemMapRatingsVector.get(itemUserId), userRating, itemUserRating)
          //itemIdUserIdHash += itemUserId -> addToList[Int](itemIdUserIdHash.get(itemUserId), userId)
        }
      }

      purchasedTogether ++= localPurchasedTogether
      SimilarItems.calculateSimilarity(itemId, itemMapRatingsVector) 
    }

    Future.value(createHttpResponse("done"))
  }

  def addToVector(vectors: Option[(RealVector, RealVector)], userRating: Double, itemUserRating: Double): (RealVector, RealVector) = {
    if(vectors != None) {
      val vec1 = vectors.get._1.append(userRating)
      val vec2 = vectors.get._2.append(itemUserRating)
      (vec1, vec2)
    }
    else {
      (new ArrayRealVector(Array(userRating)), new ArrayRealVector(Array(itemUserRating)))
    }
  }

  // userItems -> SimilarItems - userItems -> calculate predictions
  //todo improve this with k-nearest
  def getCalculateUserPrediction(userId: Int, path: Array[String]): Future[HttpResponse] = {
    if(path.size > 0) {
      val itemId = path.head.toInt
      val ratingsSimilarities = Ratings.byUserIdItemIdWithSimilarItem(userId, itemId)

      val numerator = ratingsSimilarities.map({case (rating, similarity) => {
        rating * similarity
      }}).sum 
      val denumerator = ratingsSimilarities.map(_._2).sum
      val result = if(denumerator == 0) 0 else (numerator / denumerator)

      //calculate prediction for a specific item
      Future.value(createHttpResponse(""+result))
    }

    else {

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
