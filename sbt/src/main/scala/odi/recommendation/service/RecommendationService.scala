package odi.recommendation

import org.jboss.netty.handler.codec.http.{HttpResponse}
import com.twitter.finagle.builder.Server
import com.twitter.util.{Promise, Future}

/** this service controls the item based and svd based recommendation service **/
object RecommendationService extends HttpServer {
  val name = "RecommendationService"
  SVDBasedService(Services("svdBasedService").toInt)
  val svdClient = new HttpClient("localhost:"+Services("svdBasedService"))
  ItemBasedService(Services("itemBasedService").toInt)
  val itemClient = new HttpClient("localhost:"+Services("itemBasedService"))


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
      case "generateRecommendations" => postGenerateRecommendations(path.tail, value)
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
      case "calculateSimilarities" => getCalculateSimilarities(path.tail)
      case "calculateUserPrediction" => getCalculateUserPrediction(path.tail.head.toInt, path.tail)
      case "calculateUserPredictionItemBased" => getCalculateUserPredictionItemBased(path.tail.head.toInt, path.tail)
      case _ => Future.value(createHttpResponse("No such method RecommendationService"))
    }
  }

  /** run this query every 24h
    * calculate similar users and similar items save them in SimilarUsers and SimilarItems
    * calculate average rating
    */
  def getCalculateSimilarities(path: Array[String]): Future[HttpResponse] = {
    val ret = new Promise[HttpResponse]
    Users.calculateAverageRating
    Items.calculateAverageRating
    itemClient.get("/calculateSimilarItems/") onSuccess { value =>
      val displayMatrix = if(path.size > 0 && path.head == "matrix") "matrix" else ""
      svdClient.get("/calculateSimilarUsers/"+displayMatrix) onSuccess { v =>
        ret.setValue(createHttpResponse(v))
      }
    }
    ret
  }

  /* take all similar users, get their best rated items that are unknown to the user, sort them by rating */
  def getCalculateUserPrediction(userId: Int, path: Array[String]): Future[HttpResponse] = {
    val ret = new Promise[HttpResponse]
    svdClient.get("/calculateUserPrediction/"+userId) onSuccess { v =>
      ret.setValue(createHttpResponse(v))
    }
    ret
  }

  /** calculate a user prediction with the item based recommendation system */
  def getCalculateUserPredictionItemBased(userId: Int, path: Array[String]): Future[HttpResponse] = {
    val ret = new Promise[HttpResponse]
    itemClient.get("/calculateUserPrediction/"+userId) onSuccess { v =>
      ret.setValue(createHttpResponse(v))
    }
    ret
  }

  /** generate recommendations for a user with the svd recommendation system */
  def postGenerateRecommendations(args: Array[String], value: String): Future[HttpResponse] = {
    if(args.length > 1) {
      val ret = new Promise[HttpResponse]
      val userId = args(0)
      val amount = args(1)
      svdClient.post("/generateRecommendations/"+userId+"/"+amount, value) onSuccess { v =>
        ret.setValue(createHttpResponse(v))
      }
      ret
    }
    else {
      Future.value(createHttpResponse("not enough paramater"))
    }
  }
}

