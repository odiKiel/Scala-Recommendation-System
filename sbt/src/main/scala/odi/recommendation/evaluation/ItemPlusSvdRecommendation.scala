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
    (1 to 1682).foreach(f => Items.create(Item(None, "Item "+f)))
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
}

