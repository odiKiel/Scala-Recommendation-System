package odi.recommendation
import org.jboss.netty.handler.codec.http.{HttpResponse}
import com.twitter.finagle.builder.Server
import com.twitter.util.{Promise, Future}


/** this service calculates the user ratings if a user visits a website */ 
object RatingService extends HttpServer {

  val name = "RatingService"
  val taggerService = TaggerService(Services("taggerService").toInt)
  val tagClient = new HttpClient("localhost:"+Services("taggerService"))

  /** start the service
    * @param port of the service
    * @return the port that the service runs on
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
      case "currentItem" => postCurrentItem(path.tail, value)
      case "calculateRating" => postCalculateRating(path.tail, value)
      case _ => Future.value(createHttpResponse("No such method"))
    }
  }

  /** this method is called by the HttpServer router 
    * and forwards the request to the correct get method
    * @param path the path that the request is send to
    * @return it returns a future http request
    */
  def callGetMethod(path: Array[String]): Future[HttpResponse] = {
    path.head match {
      case _ => Future.value(createHttpResponse("No such method"))
    }
  }

  /** an item that a user currently visits
    *
    * create item and user if they do not exist 
    * @param args the path of the request
    * @param value a json string with the item id, the user id, title and text of the item
    */
  def postCurrentItem(args: Array[String], value: String): Future[HttpResponse] = {
    println("args: "+args)
    println("value: "+Json.jsonToList(value))
    if(args.length > 1) {
      val item = Items.byQId(args(0).toInt)
      val user = Users.byQUserId(args(1).toInt)
      if(user == None) {
        Users.create(User(None, "User "+args(1), 0.0, args(1).toInt))
      }
      if(item == None) {
        val list = Json.jsonToList(value)
        if(list.length == 3) {
          createItem(args(0).toInt, list(0), list(1), list(2))
        }
        else {
          Future.value(createHttpResponse("Not enough parameter"))
        }
      }
      else {
        Future.value(createHttpResponse("OK"))
      }
    }
    else {
      Future.value(createHttpResponse("Not enough parameter"))
    }
  }


  /** calculate the rating for the time a user spends and the time a user scrolls on a website
    *
    * @param args the path of the request
    * @param value a json string that includes the user id the item id the time a user scrolled and spend on the website
    */
  def postCalculateRating(args: Array[String], value: String): Future[HttpResponse] = {
    println("value: "+value)
    val list = Json.jsonToList(value)
    println(list)
    println(list.length)
    if(list.length > 4) {
      val timeSpend = list(0).toDouble
      val timeScroll = list(1).toDouble
      val item = Items.byQId(list(2).toInt)
      val user = Users.byQUserId(list(3).toInt)
      val userInteraction = (list(4).toInt == 1)
      if(user != None && item != None) {
        val itemUserTime = ItemUserTimes.create(ItemUserTime(None, item.get.id.get, user.get.id.get, timeSpend, timeScroll))
        Ratings.calculateRatingByTimes(item.get, user.get.id.get, itemUserTime.timeSpend, itemUserTime.timeScroll, userInteraction)
        Future.value(createHttpResponse("OK"))
      }
      else {
        Future.value(createHttpResponse("user or item is missing"))
      }
    }
    else Future.value(createHttpResponse("Not enough parameter"))
  }

  /** creates a new item for the new question */
  def createItem(itemId: Int, text: String, title: String, url: String) = {
    println("trying to create item")
    val item = Items.create(Item(None, title, 0.0, url, itemId, 20763, 5339, 11)) //the last three numbers are for testing purpose only otherwise the system need at least three ratings befor it recommends this item
    tagClient.post("/prefLabelText", text) onSuccess {response => 
      println("descriptor: "+response)
      val prefLabels = Json.jsonToList(response)
      prefLabels.foreach(prefLabel => item.addTag(prefLabel))
    }
    Future.value(createHttpResponse("Item created"))
  }

}

