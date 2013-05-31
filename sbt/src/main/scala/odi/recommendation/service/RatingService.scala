package odi.recommendation
import org.jboss.netty.handler.codec.http.{HttpResponse}
import com.twitter.finagle.builder.Server
import com.twitter.util.{Promise, Future}


//site has rating.js
//
/*
<script type='text/javascript'>
var _rating = _rating || [];
_rating.push(['user', 'UA-25122923-1']);
_rating.push(['item', '23']);
_rating.push(['tags', []]);
_rating.push(['button', ['css tag', 'css tag']]);
_rating.push(['information', ['css tag', 'css tag']);
</script>
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
      case "currentItem" => postCurrentItem(path.tail.head.toInt)
      case "curentItemDetails" => postCurrentItemDetail(path.tail)
      case "calculateRating" => postCalculateRating(path.tail)
      case _ => Future.value(createHttpResponse("No such method"))
    }
  }

  def callGetMethod(path: Array[String]): Future[HttpResponse] = {
    path.head match {
      case _ => Future.value(createHttpResponse("No such method"))
    }
  }

  def postCurrentItem(qItemId: Int): Future[HttpResponse] = {
    val item = Items.byQId(qItemId)
    if(item == None) {
      createItemResponse
    }
    else {
      Future.value(createHttpResponse("OK"))
    }
  }

  def postCalculateRating(args: Array[String]) = {
    if(args.length > 1) {
      val spendTime = args(0).toDouble
      val timeScroll = args(1).toDouble
      // save times if item has averageTimeSpend and averageTimeScroll calculate rating if not add times check if 10 < entries and calculate
      Future.value(createHttpResoponse("OK"))
    }
    else Future.value(createHttpResponse("Not enough parameter"))
  }

  /** creates a new item for the new question */
  def postCurrentItemDetail(args: Array[String]) = {
    if(args.length > 3) {
      val qId = args(0).toInt
      val url = args(1)
      val title = args(2)
      val text = args(3) + " "+title
      val item = Items.create(Item(None, title, 0.0, url, qId))
      tagClient.post("/prefLabelText", text) onSuccess {response => {
        val prefLabels = Json.jsonToList(response)
        prefLabels.foreach(prefLabel => item.addTag(prefLabel))
      }}
      Future.value(createHttpResponse("Item created"))
    }
    else {
      Future.value(createHttpResponse("Not enough parameter for creating a new item"))
    }

    //get question id, url and list of tags
  }

  def addTagToItem = {
  }

  /** returns a javascript code that reads the current item details */
  def createItemResponse: Future[HttpResponse] = {
    //read javascript file return it as HttpResponse
    Future.value(createHttpResponse("not implemented"))
  }

}

