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
_rating.push(['tags', []]);
</script>
*/ 
object RatingService extends HttpServer {

  val name = "RatingService"

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
      case _ => Future.value(createHttpResponse("No such method"))
    }
  }

  //attention used .get() here todo improve this!
  def postTagText(args: Array[String], value: String): Future[HttpResponse] = {
    val r = new Promise[HttpResponse]
//  tagText(value) onSuccess { tags => 
//    r.setValue(createHttpResponse(Json.toJson(tags.toList)))
//  }
    r
  }

  def getTagWord(value: String): Future[HttpResponse] = {
    //run levenshtein distance algorithm with the word and all stw thesaurus words
    Future.value(createHttpResponse("string"))
  }
}

