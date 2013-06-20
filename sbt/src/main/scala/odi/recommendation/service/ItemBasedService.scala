package odi.recommendation
import org.jboss.netty.handler.codec.http.{HttpResponse}
import com.twitter.finagle.builder.Server
import com.twitter.util.{Promise, Future}
import org.apache.commons.math3.linear._


/** the item based recommendation service */
object ItemBasedService extends HttpServer with ListOperation {
  val name = "ItemBasedService"

  /** start the item based service on the port
    *
    * @param port the port that the service should wait for requests on
    * @return the port of the service
    */
  def apply(port: Int): Int = {
    super.apply(port, name)
  }

  /** this method is called by the HttpServer router 
    * and forwards the request to the correct post method
    * @param path the path that the request is send to
    * @param value the value of the post body
    * @return it returns a future http request
    */
  def callPostMethod(path: Array[String], value: String): Future[HttpResponse] = {
    path.head match {
      case _ => Future.value(createHttpResponse("No such method"))
    }
  }

  /** this method is called by the HttpServer router 
    * and forwards the request to the correct get method
    * @param path the path that the request is send to
    * @return it returns a future http request
    */
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

  /** calculate the item similarities
    *
    * for each item that is connectet with a user find all items from the user and
    * calculate the similarity with the original item
    * @param path the path of the request this is not used
    * @return a future httprequest
    */
  def getCalculateSimilarItems(path: Array[String]): Future[HttpResponse] = {
    val items: List[Int] = Items.allIds
    SimilarItems.deleteAll
    val time = System.nanoTime
    val purchasedTogether = collection.mutable.Set[(Int, Int)]() //all items that where purchased together by one or more users

    for(itemId: Int <- items) {
      val itemMapRatingsVector = collection.mutable.HashMap[Int, (RealVector, RealVector)]()
      val localPurchasedTogether = collection.mutable.Set[(Int, Int)]() //all items that where purchased together with the current item use this for easy creation of purchasedTogether
      for((userId: Int, userRating: Double) <- Users.userIdsForItemIdWithRatingNormalized(itemId);
          (itemUserId: Int, itemUserRating: Double) <- Items.allItemIdsForUserIdWithRatingNormalized(userId))
      {
        if(itemId != itemUserId && !(purchasedTogether.contains((itemId, itemUserId)) || purchasedTogether.contains((itemUserId, itemId)))) {
          localPurchasedTogether += ((itemId, itemUserId))
          itemMapRatingsVector += itemUserId -> addToVector(itemMapRatingsVector.get(itemUserId), userRating, itemUserRating)
        }
      }

      purchasedTogether ++= localPurchasedTogether
      SimilarItems.calculateSimilarity(itemId, itemMapRatingsVector) 
    }

    println("total time: "+(System.nanoTime-time))
    Future.value(createHttpResponse("done"))
  }

  /** adds a new entry to a vector
    * @param vectors a tuple of vectors
    * @param userRating the rating that should be added to the vector
    * @param itemUserRating the itemId that corresponds with the user Rating
    * @return the new tuple of vectors
    */
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

  /** if the path contains an item id predict the rating for user and item
    * if not predict the ratings for all items 
    *
    * @param userId the user id that the prediction is for
    * @param path the path of the requests if it includes an item id use it for prediction
    *        otherwise create predictions for all users
    */
  def getCalculateUserPrediction(userId: Int, path: Array[String]): Future[HttpResponse] = {
    //calculate prediction for one item?
    if(path.size > 0) {
      val itemId = path.head.toInt
      val ratingsSimilarities = Ratings.byUserIdItemIdWithSimilarItem(userId, itemId)

      val numerator = ratingsSimilarities.map({case (itemId, rating, similarity) => {
        rating * similarity
      }}).sum 
      val denumerator = ratingsSimilarities.map(_._2).sum
      val result = if(denumerator == 0) 0 else (numerator / denumerator)

      //calculate prediction for a specific item
      Future.value(createHttpResponse(""+result))
    }

    //calculate predictions for all items
    else {
      val time = System.nanoTime

      //save the items that are unknown together with the item that are known and their similarity values
      val similarItems = collection.mutable.HashMap[Int, List[(Int, Double, Int)]]()
      val allItemsUser = Items.allItemIdsUserId(userId)

      for(userItemId: Int <- allItemsUser;
          (similarItemId, rating, similarity) <- Ratings.byUserIdItemIdWithSimilarItem(userId, userItemId)) //take top 25 
      {
        if(!allItemsUser.contains(similarItemId) && similarity > 0) {//item is unknown to the user and similarity is not independence
          similarItems += similarItemId -> addToList[(Int, Double, Int)](similarItems.get(similarItemId), (userItemId, similarity, rating)) //add new item, similarity pair to the item that is to be calculated

        }
      }

      var i = 0
      val recommendations = similarItems.flatMap({case (itemId: Int, similarItemList: List[(Int, Double, Int)]) => {
          i += 1
          Map(itemId.toString -> calculatePrediction(userId, similarItemList).toString)
        }  
      })

      println("total time: "+(System.nanoTime - time))
      Future.value(createHttpResponse(Json.toJson(recommendations)))
    }
  }

  /** calculate the prediction for one item from one User by the items that he already rated
    *
    * @param userId the user that the prediction is for
    * @param similarItems the items that that the user rated together with the items that he did not rated
    */
  def calculatePrediction(userId: Int, similarItems: List[(Int, Double, Int)]): Double = {
    similarItems.length match {
      case 0 => 0
      case _ => {
        val numerator = similarItems.map({case (itemId, similarity, rating) => {
          rating * similarity
        }}).sum 
        (numerator / similarItems.map(_._2).sum)
      }
    }
  }

}
