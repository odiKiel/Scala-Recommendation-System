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
  def getCalculateSimilarItems(path: Array[String]): Future[HttpResponse] = {
    val items: List[Item] = Items.all
    val purchasedTogether = collection.mutable.Set[(Item, Item)]() //all items that where purchased together by one or more users

    for(item: Item <- items) {
      val itemUserHash = collection.mutable.HashMap[Item, List[User]]()
      val localPurchasedTogether = collection.mutable.Set[(Item, Item)]() //all items that where purchased together with the current item use this for easy creation of purchasedTogether
      for(user: User <- Users.usersForItem(item);
          itemUser: Item <- Items.allItemsUser(user.id.get))
      {
        if(item != itemUser && !(purchasedTogether.contains((item, itemUser)) || purchasedTogether.contains((itemUser, item)))) {
          localPurchasedTogether += ((item, itemUser))
          itemUserHash += itemUser -> addToList[User](itemUserHash.get(itemUser), user)
        }
      }

      purchasedTogether ++= localPurchasedTogether
      SimilarItems.calculateSimilarity(item, itemUserHash) 
    }

    Future.value(createHttpResponse("done"))
  }



  // userItems -> SimilarItems - userItems -> calculate predictions
  //todo improve this with k-nearest
  def getCalculateUserPredictions(userId: Int, path: Array[String]): Future[HttpResponse] = {

    //save the items that are unknown together with the item that are known and their similarity values
    val similarItems = collection.mutable.HashMap[Item, List[(Item, Double)]]()
    val allItemsUser = Items.allItemsUser(userId)

    for(userItem: Item <- allItemsUser;
        similarItem: (Item, Double) <- userItem.similarItems) 
    {
      if(!allItemsUser.contains(similarItem._1) && similarItem._2 != 0) {//item is unknown to the user and similarity is not independence
        similarItems += similarItem._1 -> addToList[(Item, Double)](similarItems.get(similarItem._1), (userItem, similarItem._2))

      }
    }

    val recommendations = similarItems.flatMap({case (item, similarList) => Map(item.id.get.toString -> calculatePrediction(userId, similarList.toList).toString)})

    Future.value(createHttpResponse(Json.toJson(recommendations)))
  }

  //calculate the prediction for one item from one User by the items that he already rated
  def calculatePrediction(userId: Int, similarItems: List[(Item, Double)]): Double = {
    similarItems.length match {
      case 0 => 0
      case _ => {
        val numerator = similarItems.map({case (item, similarity) => {
          Ratings.getByItemUser(item.id.get, userId).get.rating * similarity
        }}).sum 
        (numerator / similarItems.map(_._2).sum)
      }
    }
  }

}
