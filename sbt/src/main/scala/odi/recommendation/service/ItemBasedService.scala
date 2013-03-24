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
      case _ => Future.value(createHttpResponse("No such method"))
    }
  }

  //for each item that is connectet with a user find all items from the user and calculate the similarity with the original item
  //should work with a futurepool
  def getCalculateSimilarItems(path: Array[String]): Future[HttpResponse] = {
    val items: List[Item] = Items.getAll().getOrElse(List[Item]())
    val purchasedTogether = collection.mutable.Set[(Item, Item)]() // purchasedTogether(x, _) || purchasedTogether(_, x)
    for(item: Item <- items) {
      val itemUserHash = collection.mutable.HashMap[Item, collection.mutable.LinkedList[User]]()
      for(users: List[User] <- Users.usersForItem(item);
        user: User <- users;
        itemsUser: List[Item] <- Items.allItemsUser(user);
        itemUser: Item <- itemsUser) 
      {
        if(!(purchasedTogether(item, itemUser) || purchasedTogether(itemUser, item))) {//do I get all connection with this?
          if(itemUserHash.contains(itemUser)) {
            itemUserHash(itemUser).append(collection.mutable.LinkedList(user))
          }
          else {
            itemUserHash += itemUser -> collection.mutable.LinkedList(user)
          }
        }
      }
      SimilarItems.calculateSimilarity(item, itemUserHash) //need the rating id
    }

    Future.value(createHttpResponse("done"))
  }

}
