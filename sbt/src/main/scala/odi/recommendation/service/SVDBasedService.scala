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
    path.head match {
      case "calculateSimilarUsers" => getCalculateSimilarUsers(path)
      case "calculateUserPredictions" => getCalculateUserPredictions(path.tail.head.toInt, path.tail)
      case _ => Future.value(createHttpResponse("No such method"))
    }
  }

  /*
   calculate similar users and save them in SimilarUsers
   */
  def getCalculateSimilarUsers(path: Array[String]): Future[HttpResponse] = {
    val userItemMatrix = createUserItemMatrix
    val svd = calculateSVD(userItemMatrix)
    val users2D = u2d(svd)
    SimilarUsers.calculateSimilarity(users2D)
    Future.value(createHttpResponse(Json.toJson(users2D)))
  }

  /*
   take all similar users, get their best rated items that are unknown to the user, sort them by rating
   */
  //use only ratings that aren't predictions!
  def getCalculateUserPredictions(userId: Int, path: Array[String]): Future[HttpResponse] = {
    val predictions = collection.mutable.HashMap[Int, List[(Int, Double)]]()
    for(s <- SimilarUsers.byUserId(userId, 5)){
      val (similarUser, similarity) = s.similarityByUserId(userId).get
      for(rating <- Ratings.getUnknownItemsForUserByUser(userId, similarUser.id.get))
      {
        predictions += rating.itemId -> addToList(predictions.get(rating.itemId), (rating.rating, similarity))
      }
    }
    val predictionMap = predictions.flatMap((i: (Int, List[(Int, Double)])) => Map(i._1.toString->calculatePrediction(i._2).toString))
    Future.value(createHttpResponse(Json.toJson(predictionMap)))
  }

  def calculatePrediction(topItems: List[(Int, Double)]): Double = {
    val numerator = topItems.map((i: (Int, Double)) => i._1 * i._2).sum
    var value = (numerator / topItems.map(_._2).sum)
    if(value < 0) 0 else value
  }
  /*

   Array[Array[Int]](Array[Int](1, 2, 3, 4, 5), Array[Int](2, 3, 4, 5, 5), Array[Int](5, 4, 3, 2, 1), Array[Int](5, 5, 4, 3, 2), Array[Int](3, 3, 3, 3, 3))
   */
  def createUserItemMatrix: Array[Array[Int]] = {
    //each item each user search for rating if not enter 0 tons of db queries
    val allUsers = Users.all
    val matrix = Array.fill(allUsers.length){collection.mutable.ListBuffer[Int]()}
    val allItems: List[Item] = Items.all
    for(item <- allItems;
        (user, i) <- Users.all.zipWithIndex)
    {  
      val rating = Ratings.getByItemUser(item.id.get, user.id.get)
      if(rating != None) {
        matrix(i) += rating.get.rating
      }
      else {
        matrix(i) += 0
      }
    }
    matrix.map(_.toArray)
  }

  def u2d(svd: SingularValueDecomposition): Array[Array[Double]] = {
    val u = svd.getU()
    val uColumnSize = u.getColumn(0).length
    
    u.getSubMatrix(0, uColumnSize-1, 0, 1).getData()
  }

  //calculate the singular value decomposition for an array of int
  def calculateSVD(matrixInt: Array[Array[Int]]): SingularValueDecomposition = {
    val matrixDouble = matrixInt.map((row: Array[Int]) => row.map(_.toDouble))
    val realMatrix = MatrixUtils.createRealMatrix(matrixDouble)
    new SingularValueDecomposition(realMatrix)
  }
}

