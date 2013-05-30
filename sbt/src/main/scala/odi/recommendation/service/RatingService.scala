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

  def apply(port: Int): Int = {
    super.apply(port, name)
  }

  def callPostMethod(path: Array[String], value: String): Future[HttpResponse] = {
    path.head match {
      case "currentItem" => postCurrentItem(path.tail.head.toInt)
      case _ => Future.value(createHttpResponse("No such method"))
    }
  }

  def callGetMethod(path: Array[String]): Future[HttpResponse] = {
    path.head match {
      case _ => Future.value(createHttpResponse("No such method"))
    }
  }

  def postCurrentItem(itemId: Int): Future[HttpResponse] = {
    Future.value(createHttpResponse("test"))
  }
}

