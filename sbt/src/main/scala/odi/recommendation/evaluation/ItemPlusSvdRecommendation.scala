package odi.recommendation

import org.jboss.netty.handler.codec.http.{HttpResponse}
import com.twitter.finagle.builder.Server
import com.twitter.util.{Promise, Future}
import java.io._
import com.github.tototoshi.csv.CSVReader

object ItemPlusSvdRecommendation {
  //RecommendationService(Services("recommendationService").toInt)
  //val recommendationClient = new HttpClient("localhost:"+Services("recommendationService"))

  def apply(port: Int): Int = {
    1
  }

  //load the movielens test database into the database
  def loadMovieLens = {
    Ratings.deleteAll
    SimilarItems.deleteAll
    SimilarUsers.deleteAll
    Items.deleteAll
    Users.deleteAll

    val reader = CSVReader.open(new File("ml-100k/u1base.csv"))

    (1 to 943).foreach(f => Users.create(User(None, "User "+f, 0.0)))
    (1 to 1682).foreach(f => Items.create(Item(None, "Item "+f, 0.0)))
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

  def calculateSimilarities = {
  }

  def calculateSimilarUsers = {
  }

  def calculateSimilarItems = {
  }

  def evaluate = {
    val testData = loadTestData
    println("done with loadingTestData")
    var i=0
    val time = System.nanoTime
    val mae = testData.foldLeft[Double](0.0)((s, c) => {
        i+=1
        println("Prediction number: "+i)
        comparePrediction(c)+s
      })
    println("Sum: "+mae+" MAE: "+(mae / testData.length)+" time: "+(System.nanoTime-time))
  }

  //testData (userId, itemId, rating)
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

  //testEntry (userCsvId, itemCsvId, rating)
  def comparePrediction(testEntry: (Int,  Int, Int)): Double = {
    comparePredictionSvdBased(testEntry)
    //comparePredictionItemBased(testEntry)
    //comparePredictionOfThree(testEntry)
  }


  def comparePredictionSvdBased(testEntry: (Int,  Int, Int)): Double = {
    val res = SVDBasedService.getCalculateUserPrediction(testEntry._1, Array(""+testEntry._2)).get().getContent().toString("UTF-8").toDouble
    println("result: "+res+" expected: "+testEntry._3)
    Math.abs(res - testEntry._3)
  }

  def comparePredictionItemBased(testEntry: (Int,  Int, Int)): Double = {
    val res = ItemBasedService.getCalculateUserPrediction(testEntry._1, Array(""+testEntry._2)).get().getContent().toString("UTF-8").toDouble
    println("result: "+res+" expected: "+testEntry._3)
    Math.abs(res - testEntry._3)
  }

  def comparePredictionOfThree(testEntry: (Int, Int, Int)): Double = {
    Math.abs(3 - testEntry._3)
  }

}

