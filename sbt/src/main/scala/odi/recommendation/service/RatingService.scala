package odi.recommendation
import org.jboss.netty.handler.codec.http.{HttpResponse}
import com.twitter.finagle.builder.Server
import com.twitter.util.{Promise, Future}


//site has rating.js
//
/*
 $(".question-body>p").text()
<script type='text/javascript'>
var _rating = _rating || [];
_rating.push(['user', '{{user.id}}']);
_rating.push(['item', '{{question.id}}']);
_rating.push(['button', ['#add-answer-btn', '.post-vote']]);
_rating.push(['information', '.question-body>p');
_rating.push(['title', ['h1']);
</script>
_rating.push(['tags', []]); // nur bei der zu empfehlenen site
*/ 
object RatingService extends HttpServer {

  val name = "RatingService"
  val taggerService = TaggerService(Services("taggerService").toInt)
  val tagClient = new HttpClient("localhost:"+Services("taggerService"))

  def apply(port: Int): Int = {
    super.apply(port, name)
  }

  def callPostMethod(path: Array[String], value: String): Future[HttpResponse] = {
    path.head match {
      case "currentItem" => postCurrentItem(path.tail, value)
      //case "curentItemDetails" => postCurrentItemDetail(path.tail, value)
      case "calculateRating" => postCalculateRating(path.tail, value)
      case _ => Future.value(createHttpResponse("No such method"))
    }
  }

  def callGetMethod(path: Array[String]): Future[HttpResponse] = {
    path.head match {
      case _ => Future.value(createHttpResponse("No such method"))
    }
  }

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
        if(list.length == 4) {
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
    val item = Items.create(Item(None, title, 0.0, url, itemId, 0.0, 0.0, 0))
    tagClient.post("/prefLabelText", text) onSuccess {response => {
      val prefLabels = Json.jsonToList(response)
      prefLabels.foreach(prefLabel => item.addTag(prefLabel))
    }}
    Future.value(createHttpResponse("Item created"))
  }

  /** returns a javascript code that reads the current item details 
    *
    * not in use it is cross site scripting and not allowed by browsers
    */
  def createItemResponse: Future[HttpResponse] = {
    //read javascript file return it as HttpResponse
    val source = scala.io.Source.fromFile("public/js/rating/itemdetails.js")
    val lines = source.getLines mkString "\n"
    source.close()
    println(lines)
    
    Future.value(createHttpResponse(lines))
  }

}

