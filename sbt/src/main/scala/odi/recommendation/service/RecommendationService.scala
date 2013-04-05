package odi.recommendation

import org.jboss.netty.handler.codec.http.{HttpResponse}
import com.twitter.finagle.builder.Server
import com.twitter.util.{Promise, Future}

object RecommendationService extends HttpServer {
  val name = "RecommendationService"
  SVDBasedService(Services("svdBasedService").toInt)
  val svdClient = new HttpClient("localhost:"+Services("svdBasedService"))
  ItemBasedService(Services("itemBasedService").toInt)
  val itemClient = new HttpClient("localhost:"+Services("itemBasedService"))

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
      case "calculateSimilarities" => getCalculateSimilarities(path)
      case "calculateUserPredictions" => getCalculateUserPredictions(path.tail.head.toInt, path.tail)
      case _ => Future.value(createHttpResponse("No such method"))
    }
  }

  /*
   calculate similar users and similar items save them in SimilarUsers and SimilarItems
   */
  def getCalculateSimilarities(path: Array[String]): Future[HttpResponse] = {
    val ret = new Promise[HttpResponse]
    itemClient.get("/calculateSimilarItems/") onSuccess { value =>
      println("item return: "+value)
      svdClient.get("/calculateSimilarUsers/") onSuccess { v =>
        println("svd return"+v)
        ret.setValue(createHttpResponse(v))
      }
    }
    ret
  }

  /*
   take all similar users, get their best rated items that are unknown to the user, sort them by rating
   */
  def getCalculateUserPredictions(userId: Int, path: Array[String]): Future[HttpResponse] = {
    val ret = new Promise[HttpResponse]
    svdClient.get("/calculateUserPredictions/"+userId) onSuccess { v =>
      ret.setValue(createHttpResponse(v))
    }
    ret
  }
}

