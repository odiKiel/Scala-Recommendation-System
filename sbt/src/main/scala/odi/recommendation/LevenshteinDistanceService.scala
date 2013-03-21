package odi.recommendation
import math._
import org.jboss.netty.handler.codec.http.{HttpResponse}
import com.twitter.finagle.builder.Server
import com.twitter.util.{Promise, Future}
import net.liftweb.json._


//distance small words max 1 medium words 2 large words 3
object LevenshteinDistanceService extends HttpServer {

  val name = "LevenshteinDistanceService"
  def apply(port: Int): Int = {
    super.apply(port, name)
  }

  def callPostMethod(path: Array[String], value: String): Future[HttpResponse] = {
    Future.value(path.head match {
      case "labelForText" => postLabelForText(path.tail(0), Json.jsonToList(value))
      case _ => createHttpResponse("No such method")
    })
  }

  def callGetMethod(path: Array[String]): Future[HttpResponse] = {
    Future.value(path.head match {
      case "distanceForWords" => getDistanceForWords(path(0).toString, path(1).toString)
      case _ => createHttpResponse("No such method")
    })
  }

  def postLabelForText(label: String, text: List[String]): HttpResponse = {
    var hasMatch: Option[String] = None
    var currentList = text
    while(hasMatch == None && currentList.length > 0) {
      hasMatch = checkForMatch(label, currentList.head)
      currentList = currentList.tail
    }
    createHttpResponse(hasMatch.getOrElse(""))
  }

  def checkForMatch(label: String, word: String): Option[String] = {
    if(calcLevDist(label, word) < 4) {
      Some(label)
    }
    else {
      None
    }
  }

  def getDistanceForWords(word1: String, word2: String): HttpResponse = {
    createHttpResponse(calcLevDist(word1, word2).toString)
  }


  def calcLevDist(s:String, t:String):Int = {
    var cost = 0


    if(s.length() == 0) {
      return t.length()
    }
    else if(t.length() == 0) {
      return s.length
    }

    if(s.last != t.last) {
      cost = 1
    }

    math.min(calcLevDist(s, t.substring(0, t.length()-1))+1, math.min(calcLevDist(s.substring(0, s.length()-1), t)+1, calcLevDist(s.substring(0, s.length()-1), t.substring(0, t.length()-1))+cost))
  }


}


