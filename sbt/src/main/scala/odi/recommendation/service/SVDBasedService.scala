package odi.recommendation

import org.jboss.netty.handler.codec.http.{HttpResponse}
import com.twitter.finagle.builder.Server
import com.twitter.util.{Promise, Future}
import org.apache.commons.math3.linear._

object SVDBasedService extends HttpServer with ListOperation {
  val name = "SVDBasedService"

  def apply(port: Int): Int = {
    super.apply(port, name)
  }

  def callPostMethod(path: Array[String], value: String): Future[HttpResponse] = {
    path.head match {
      case _ => Future.value(createHttpResponse("No such method"))
    }
  }

  def callGetMethod(path: Array[String]): Future[HttpResponse] = {
    if(path.size < 1) Future.value(createHttpResponse("Not enough parameter for ItemBasedService"))
    else {
      path.head match {
        case "calculateSimilarUsers" => getCalculateSimilarUsers(path.tail)
        case "calculateUserPrediction" => {
          if(path.size > 1) getCalculateUserPrediction(path.tail.head.toInt, path.tail.tail)
          else Future.value(createHttpResponse("Not enough parameter for prediction calculation"))
        }
        case _ => Future.value(createHttpResponse("No such method"))
      }
    }
  }

  /*
   calculate similar users and save them in SimilarUsers
   */
  def getCalculateSimilarUsers(path: Array[String]): Future[HttpResponse] = {
    SimilarUsers.deleteAll
    val matrix = calculateSimilarUsers
    if(path.head == "matrix") {
      Future.value(createHttpResponse(Json.toJson(matrix.getData)))
    }
    else {
      Future.value(createHttpResponse("Done"))
    }
  }

  def calculateSimilarUsers: RealMatrix = {
    SimilarUsers.deleteAll
    Users.calculateAverageRating
    val time = System.nanoTime
    println("start with calculations")
    val userItemMatrix = createUserItemMatrix
    println("done with createUserItemMatrix")
    val svd = calculateSVD(userItemMatrix)
    println("done with svd calculation")
    println("time after svd: "+(System.nanoTime-time))
    val users2D = u2d(svd)
    println("done with getting u2d")
    SimilarUsers.calculateSimilarity(users2D)
    println("done with calculateSimilarity")
    println("total time: "+(System.nanoTime-time))
    users2D
  }

  /*
   take all similar users, get their best rated items that are unknown to the user, sort them by rating
   only use users with similarities > 0
   */
  //use only ratings that aren't predictions!
  def getCalculateUserPrediction(userId: Int, path: Array[String]): Future[HttpResponse] = {
    if(path.size > 0) {
      //predict rating for a specific item
      val itemId = path.head.toInt
      val user = Users.get(userId).get
      // rating, similarity, averageRating
      val ratingsSimilarities = Ratings.byUserIdItemIdWithSimilarUserAverageRating(userId, itemId)

      val numerator = ratingsSimilarities.map({case (userId, rating, similarity, averageRating) => {
        (rating-averageRating) * similarity
      }}).sum 
      val denumerator = ratingsSimilarities.map(_._3).sum
      val result = if (denumerator == 0) {
        0
      }
      else {
        val res = user.averageRating + (numerator / denumerator)
        if(res < 0) 0 else res
      }

      //calculate prediction for a specific item
      Future.value(createHttpResponse(""+result))
    }
    else {
      val predictions = collection.mutable.HashMap[Int, List[(Int, Int, Double)]]()
      for(s <- SimilarUsers.byUserId(userId, 5)){
        val (similarUserId, similarity) = s.similarityByUserId(userId).get
        if(similarity > 0) { 
          for(rating <- Ratings.getUnknownItemsForUserByUser(userId, similarUserId))
          {
            predictions += rating.itemId -> addToList(predictions.get(rating.itemId), (similarUserId, rating.rating, similarity))
          }
        }
      }
      val predictionMap = predictions.flatMap((i: (Int, List[(Int, Int, Double)])) => Map(i._1.toString->calculatePrediction(userId, i._2).toString))
      Future.value(createHttpResponse(Json.toJson(predictionMap)))
    }
  }

  def calculatePrediction(userAId: Int, topItems: List[(Int, Int, Double)]): Double = {
    if(topItems.length > 1){
      val averageRatingA = Users.get(userAId).get.averageRating
      val numerator = topItems.map{case(userBId: Int, rating: Int, similarity: Double) => {
        val averageRatingB = Users.get(userBId).get.averageRating
        (rating-averageRatingB)*similarity
      }}.sum
      val denominator = topItems.map(_._3).sum
      if(denominator == 0) 0 else averageRatingA + numerator/denominator
    }
    else 0
  }
  /*

   Array[Array[Int]](Array[Int](1, 2, 3, 4, 5), Array[Int](2, 3, 4, 5, 5), Array[Int](5, 4, 3, 2, 1), Array[Int](5, 5, 4, 3, 2), Array[Int](3, 3, 3, 3, 3))
   */
  def createUserItemMatrix: Array[Array[Double]] = {
    //each item each user search for rating if not enter 0 tons of db queries
    val allUsers = Users.all
    val allItems: List[Item] = Items.all
    val matrix = Array.fill(allUsers.length){collection.mutable.ListBuffer[Double]()}
    for((user, i) <- allUsers.zipWithIndex) {
      //all missing ratings are filled with the middle of the ratings 1-5 the three an improvement would be to use the average of that rating
      //however that would highly decrease the computation time of the algorithm
      val allItemsForUser = Ratings.allItemRatingsForUserId(user.id.get).map({case (user: Int, item: Int, rating: Option[Int], averageRating: Double) => {
          if(rating == None) averageRating.round else rating.get.toDouble
        }
      }).toArray
      matrix(i) ++= allItemsForUser
    }

    /*
     //not efficient enough
    for(item <- allItems;
        (user, i) <- Users.all.zipWithIndex)
    {  
      val rating = Ratings.byItemIdUserId(item.id.get, user.id.get)
      if(rating != None) {
        matrix(i) += rating.get.rating.toDouble
      }
      else {
        matrix(i) += 0.0
      }
    }
    */
    matrix.map(_.toArray)
  }

  def u2d(svd: SingularValueDecomposition): RealMatrix = {
    val u = svd.getU()
//  println("u:")
//  println(u)
//  println("sigma:")
//  println(svd.getS())
//  println("VT:")
//  println(svd.getVT())
    val uColumnSize = u.getColumn(0).length
    
    u.getSubMatrix(0, uColumnSize-1, 0, 1)
  }

  //calculate the singular value decomposition for an array of int
  def calculateSVD(matrix: Array[Array[Double]]): SingularValueDecomposition = {
    val realMatrix = MatrixUtils.createRealMatrix(matrix)
    new SingularValueDecomposition(realMatrix)
  }
}

