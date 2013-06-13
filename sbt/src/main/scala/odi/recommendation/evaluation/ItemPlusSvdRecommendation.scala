package odi.recommendation

import org.jboss.netty.handler.codec.http.{HttpResponse}
import com.twitter.finagle.builder.Server
import com.twitter.util.{Promise, Future}
import java.io._
import com.github.tototoshi.csv.CSVReader

/** This objects is responsible for testing the recommendation system **/
object ItemPlusSvdRecommendation {


  /** This method loads the 80.000 ratings from the movie lens database
    *
    * these 80.000 ratings are used for training the system
    * the remaining 20.000 ratings are used for testing the prediction
    */
  def loadMovieLens = {
    Ratings.deleteAll
    SimilarItems.deleteAll
    SimilarUsers.deleteAll
    Items.deleteAll
    Users.deleteAll

    val reader = CSVReader.open(new File("ml-100k/u1base.csv"))

    (1 to 943).foreach(f => Users.create(User(None, "User "+f, 0.0, f)))
    (1 to 1682).foreach(f => Items.create(Item(None, "Item "+f, 0.0, "http://www."+f".de", +f, 0.0, 0.0, 0)))
    println("created users and items")

    val itemOffset = Items.first.get.id.get-1
    val userOffset = Users.first.get.id.get-1

    var i = 1
    reader.foreach((f: Seq[String]) => {
      Ratings.create(Rating(None, f(1).toInt+itemOffset, f(0).toInt+userOffset, f(2).toInt, false))
      println("imported row: "+i)
      i+=1
    })
    reader.close()
  }

  /** calculateSimilarities starts the similarity calculations for the users and items **/
  def calculateSimilarities = {
    calculateSimilarUsers
    calculateSimilarItems
  }

  /** calculateSimilarUsers starts the similarity calculation for similar users **/
  def calculateSimilarUsers = {
    SVDBasedService.calculateSimilarUsers
  }

  /** calculateSimilarItems starts the similarity calculation for similar items **/
  def calculateSimilarItems = {
    ItemBasedService.getCalculateSimilarItems(Array(""))
  }

  /** evaluate calculates the mean absolute error for a specific recommendation technique
    * 
    * this methods loads the 20.000 ratings that are used for testing the predictions
    * it prints the time the system needs as well as the MAE
    * @param evType the recommendation technique that should be used
    *        possible selections are: svd, item, three and average
    */
  def evaluate(evType: String) = {
    val testData = loadTestData
    println("done with loadingTestData")
    var i=0
    val time = System.nanoTime
    val mae = testData.foldLeft[Double](0.0)((s, c) => {
        i+=1
        println("Prediction number: "+i)
        comparePrediction(c, evType)+s
      })
    println("Sum: "+mae+" MAE: "+(mae / testData.length)+" time: "+(System.nanoTime-time))
  }

  /** loads the 20.000 ratings from the csv test file
    *
    * @return a list of testData as a tupel (userId, itemId, rating)
    */
  def loadTestData: List[(Int, Int, Int)] = {
    val reader = CSVReader.open(new File("ml-100k/u1test.csv"))

    val itemOffset = Items.first.get.id.get-1
    val userOffset = Users.first.get.id.get-1

    val testData = collection.mutable.ListBuffer[(Int, Int, Int)]()
    reader.foreach((f: Seq[String]) => {
        val test = (f(0).toInt+userOffset, f(1).toInt+itemOffset, f(2).toInt)
      testData += test
    })
    reader.close()

    testData.toList
  }

  /** starts a prediction calculation
    *
    * @param testEntry the user, item, rating triple that should be evaluated
    * @return a value that represents the rating prediction - the original rating
    */
  def comparePrediction(testEntry: (Int,  Int, Int), evType: String): Double = {
    evType match {
      case "svd" => comparePredictionSvdBased(testEntry)
      case "item" => comparePredictionItemBased(testEntry)
      case "three" => comparePredictionOfThree(testEntry)
      case "average" => comparePredictionAverage(testEntry)
    }
  }


  /** calculates the prediction with the svd based recommendation system
    *
    * @param testEntry the user, item, rating triple that should be evaluated
    * @return a value that represents the rating prediction - the original rating
    */
  def comparePredictionSvdBased(testEntry: (Int,  Int, Int)): Double = {
    val res = SVDBasedService.getCalculateUserPrediction(testEntry._1, Array(""+testEntry._2)).get().getContent().toString("UTF-8").toDouble
    println("result: "+res+" expected: "+testEntry._3)
    Math.abs(res - testEntry._3)
  }

  /** calculates the prediction with the item based recommendation system
    *
    * @param testEntry the user, item, rating triple that should be evaluated
    * @return a value that represents the rating prediction - the original rating
    */
  def comparePredictionItemBased(testEntry: (Int,  Int, Int)): Double = {
    val res = ItemBasedService.getCalculateUserPrediction(testEntry._1, Array(""+testEntry._2)).get().getContent().toString("UTF-8").toDouble
    println("result: "+res+" expected: "+testEntry._3)
    Math.abs(res - testEntry._3)
  }

  /** uses a three as a prediction value
    *
    * @param testEntry the user, item, rating triple that should be evaluated
    * @return a value that represents the three - the original rating
    */
  def comparePredictionOfThree(testEntry: (Int, Int, Int)): Double = {
    Math.abs(3 - testEntry._3)
  }

  /** uses an average rating as a prediction value
    *
    * @param testEntry the user, item, rating triple that should be evaluated
    * @return a value that represents the average item rating - the original rating
    */
  def comparePredictionAverage(testEntry: (Int, Int, Int)): Double = {
    val res = Items.get(testEntry._2.toInt).get.averageRating
    println("result: "+res+" expected: "+testEntry._3)
    Math.abs(res - testEntry._3)
  }


}

