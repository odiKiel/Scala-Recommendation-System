package odi.recommendation
import org.jboss.netty.handler.codec.http.{HttpRequest, HttpResponse, Cookie}
import com.twitter.finagle.http.Method
import com.twitter.finagle.builder.Server
import com.twitter.util.{Promise, Future}
import java.io._
import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}

/** this service is responsible for communicating with external clients */
object WebService extends HttpServer {

  val name = "WebService"
  //for json parsing
  implicit val formats = DefaultFormats 
  TaggerService(Services("taggerService").toInt)
  val recommendationServer = RecommendationService(Services("recommendationService").toInt)
  val recommendationClient = new HttpClient("localhost:"+Services("recommendationService"))
  val ratingServer = RatingService(Services("ratingService").toInt)
  val ratingClient = new HttpClient("localhost:"+Services("ratingService"))

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
      case "data" => saveData(path.tail, value)
      case "rating" => forwardToRating(path.tail, value)
      case _ => Future.value(createHttpResponse("No such method"))
    }
  }



  /** forward all requests to the rating service 
    * @param args the path of the request
    * @param value the post body of the request
    * @return a http response
    */
  def forwardToRating(args: Array[String], value: String): Future[HttpResponse] = {
    val ret = new Promise[HttpResponse]
    ratingClient.post("/"+args.head+"/"+args.tail.mkString("/"), value) onSuccess{returnValue =>
      ret.setValue(createHttpResponse(returnValue))
    }
      
    ret
  }

  /** this method is called by the HttpServer router 
    * and forwards the request to the correct get method
    * @param path the path that the request is send to
    * @return it returns a future http request
    */
  def callGetMethod(path: Array[String]): Future[HttpResponse] = {
    path.head match {
      case "" => getFile(Array("index.html"))
      case "tagger" => getFile(Array("tagger.html"))
      case "recommend" => getFile(Array("recommendation.html"))
      case "data" => getData(path.tail)
      case "file" => getFile(path.tail)
      case "calculate" => getCalculation(path.tail)
      case "recommendation" => getRecommendations(path.tail)
      case _ => Future.value(createHttpResponse("No such method"))
    }
  }

  /** calculate the recommendations for a user and tags and return it as a javascript file
    *
    * @param args the path of the request with userId, tags and number of recommendations
    * @return httpresponse with a javascript file with recommendation links
    */
  def getRecommendations(args: Array[String]): Future[HttpResponse] = {
    val ret = new Promise[HttpResponse]
    val userIdCookie = cookies.find(cookie => cookie.getName() == "user_id")
    val userId = if(userIdCookie == None) 0 else userIdCookie.get.getValue().toInt
    println("userId: "+userId)
    val tags = args.drop(1)
    recommendationClient.post("/generateRecommendations/"+userId+"/"+args(0), Json.toJson(tags)) onSuccess { recommendationsJson => 
      val recommendations = Json.jsonToLists(recommendationsJson)
      val links = recommendations.map(entry => "<a href ='"+entry(1)+"'>"+entry(0)+" Prediction: "+entry(2)+"</a>")
      println("recommendations: "+recommendations)
      val source = scala.io.Source.fromFile("public/js/rating/recommend.js")
      val lines = source.getLines mkString "\n"
      source.close()
      val js = lines.format(links.mkString("</br>"))
      ret.setValue(createHttpResponse(js))
    }
    ret
  }

  /** calculate similarities, recommendations or recommendations item based for the test website
    *
    * @param path the path of the request
    * @return http response with the results
    */
  def getCalculation(path: Array[String]): Future[HttpResponse] = {
    path.head match {
      case "similarities" => {
        var ret = new Promise[HttpResponse]
        recommendationClient.get("/calculateSimilarities/matrix") onSuccess { v =>
          ret.setValue(createHttpResponse(v))
        }
        ret
      }
      case "recommendations" => {
        var ret = new Promise[HttpResponse]
        recommendationClient.get("/calculateUserPrediction/"+path.tail.head) onSuccess { v =>
          ret.setValue(createHttpResponse(v))
        }
        ret
      }
      case "recommendationsItemBased" => {
        var ret = new Promise[HttpResponse]
        recommendationClient.get("/calculateUserPredictionItemBased/"+path.tail.head) onSuccess { v =>
          ret.setValue(createHttpResponse(v))
        }
        ret
      }
      case _ => Future.value(createHttpResponse("No such method WebService"))
    }
  }

  /** return a static file from the public folder
    *
    * @param file the file that is requested
    * @return the file
    */
  def getFile(file: Array[String]): Future[HttpResponse] = {
    val path = file.mkString("/")
    if(path.matches("""[\/\w-]+\.\w+""")) {
      val source = scala.io.Source.fromFile("public/"+path)
      val lines = source.mkString
      source.close()
      Future.value(createHttpResponse(lines))
    }
    else {
      Future.value(createHttpResponse("not a valid path"))
    }
  }

  /** return the values of the database for the requested model this is used by the test website
    *
    * @param path one element return one element two elements return the requested element
    */
  def getData(path: Array[String]): Future[HttpResponse] = {
    path.length match {
      case 1 => getAll(path(0))
      case 2 => getOne(path(0), path(1).toInt)
      case _ => Future.value(createErrorHttpResponse)
    }
  }

  /** return all entries of the specific type
    *
    * @param kind the type of the model
    * @result all entries from the database
    */
  def getAll(kind: String): Future[HttpResponse] = {
    val response = kind match {
      case "ratings" => Ratings.all.map(_.toJson)
      case "items" => Items.all.map(_.toJson)
      case "users" => Users.all.map(_.toJson)
      case "similarItems" => SimilarItems.all.map(_.toJson)
      case "similarUsers" => SimilarUsers.all.map(_.toJson)
    }
    Future.value(createHttpResponse("["+response.mkString(", ")+"]"))
  }

  /** return a specific element from the database table
    *
    * @param kind the type of the model
    * @param id the id for the object
    * @return http response with one model
    */
  def getOne(kind: String, id: Int): Future[HttpResponse] = {
    kind match {
      case "ratings" => createHttpResponseForOption(Ratings.get(id))
      case "items" => createHttpResponseForOption(Items.get(id))
      case "users" => createHttpResponseForOption(Users.get(id))
      case "similarItems" => createHttpResponseForOption(SimilarItems.get(id))
      case "similarUsers" => createHttpResponseForOption(SimilarUsers.get(id))
    }
  }

  /** store the provided data in the database this method is used by the test website
    *
    * @param path the path of the request which specifies the type of the model
    * @param value the values that should be stored in the database
    */
  def saveData(path: Array[String], value: String): Future[HttpResponse] = {
    val data = parse(value)
    path.head match {
      case "ratings" => {
        val rating = data.extract[Rating]
        val response = Ratings.create(rating)
        Future.value(createHttpResponse(response.toJson))
      }
      case "users" => {
        val user = data.extract[User]
        val response = Users.create(user)
        Future.value(createHttpResponse(response.toJson))
      }
      case "items" => {
        val item = data.extract[Item]
        val response = Items.create(item)
        Future.value(createHttpResponse(response.toJson))
      }
      case _ => Future.value(createErrorHttpResponse)
    }
  }


  /** create a http response for an option object
    * if object exists return it othewise return an error message
    */
  def createHttpResponseForOption(e: Option[ToJson]): Future[HttpResponse] = {
    if(e == None) {
      Future.value(createErrorHttpResponse)
    }
    else {
      Future.value(createHttpResponse(e.get.toJson))
    }
  }

  /** override the routing server method 
    * this service needs put and delete requests as well
    */
  override def routing(request: HttpRequest): Future[HttpResponse] = {
    //println("new request: "+request)
    val path = request.getUri().substring(1).split("/") // remove leading / and split
    request.getMethod() match {
      case Method.Post => callPostMethod(path, request.getContent().toString("UTF-8"))
      case Method.Put => callPostMethod(path, request.getContent().toString("UTF-8"))
      case Method.Get => callGetMethod(path)
      case Method.Delete => callDeleteMethod(path, request.getContent().toString("UTF-8"))
    }

  }
 
  /** delete an object from the database
    *
    * @param path the path of the request which specifies the type that should be deleted
    * @param content is not used
    */
  def callDeleteMethod(path: Array[String], content: String): Future[HttpResponse] = {
    if(path.head == "data") {
      val model = path(1)
      val id = path(2).toInt
      val response = model match {
        case "ratings" => {
          Ratings.delete(id)
          createHttpResponse("ok")
        }
        case "items" => {
          Items.delete(id)
          createHttpResponse("ok")
        }
        case "users" => {
          Users.delete(id)
          createHttpResponse("ok")
        }
        case _ => createHttpResponse("model not found")
      }
      Future.value(response)
    }
    else {
      Future.value(createErrorHttpResponse)
    }

  }

}

