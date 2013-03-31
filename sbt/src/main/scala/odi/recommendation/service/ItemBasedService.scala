package odi.recommendation
import org.jboss.netty.handler.codec.http.{HttpResponse}
import com.twitter.finagle.builder.Server
import com.twitter.util.{Promise, Future}

object ItemBasedService extends HttpServer {
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
      case _ => Future.value(createHttpResponse("No such method"))
    }
  }

  //for each item that is connectet with a user find all items from the user and calculate the similarity with the original item
  //should work with a futurepool
  //I need purchased together otherwise all similarities are doubled because item1 -> item2 and item2 -> item1
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
          itemUserHash += itemUser -> addToList(itemUserHash.get(itemUser), user)
        }
      }

      purchasedTogether ++= localPurchasedTogether
      SimilarItems.calculateSimilarity(item, itemUserHash) 
    }

    Future.value(createHttpResponse("done"))
  }

  def addToList(userList: Option[List[User]], user: User): List[User] = {
    if(userList != None) {
      userList.get :+ user
    }
    else {
      List(user)
    }
  }


  // userItems -> SimilarItems - userItems -> calculate predictions
  //todo improve this with k-nearest
  def getCalculateUserPredictions(userId: Int, path: Array[String]): Future[HttpResponse] = {

    //save the items that are unknown together with the item that are known and their similarity values
    val similarItems = collection.mutable.HashMap[Item, collection.mutable.LinkedList[(Item, Float)]]()
    val allItemsUser = Items.allItemsUser(userId)

    for(userItem: Item <- allItemsUser;
        similarItem: (Item, Float) <- userItem.similarItems) 
    {
      if(!allItemsUser.contains(similarItem._1)) //item is unknown to the user
          if(similarItems.contains(similarItem._1)) {
            similarItems(similarItem._1).append(collection.mutable.LinkedList[(Item, Float)]((userItem, similarItem._2)))
          }
          else {
            similarItems += (similarItem._1 -> collection.mutable.LinkedList[(Item, Float)]((userItem, similarItem._2))) 
          }
    }

    val recommendations = similarItems.map({case (item, similarList) => (""+item.id.get +"#" +calculatePrediction(userId, similarList.toList))})

    Future.value(createHttpResponse(Json.listToJson(recommendations.toList)))
  }

  //calculate the prediction for one item from one User by the items that he already rated
  def calculatePrediction(userId: Int, similarItems: List[(Item, Float)]): Int = {
    val numerator = similarItems.map({case (item, similarity) => {
        Ratings.getByItemUser(item.id.get, userId).get.rating * similarity
      }}).sum 
    (numerator / similarItems.map(_._2).sum).toInt
  }

}
