package odi.recommendation
import org.jboss.netty.handler.codec.http.{HttpResponse}
import com.twitter.finagle.builder.Server
import com.twitter.util.{Promise, Future}


object TaggerService extends HttpServer {
  QueryStwServer(Services("queryStwServer").toInt)
  LevenshteinDistanceService(Services("levenshteinDistanceService").toInt)

  val queryStwClient = new HttpClient("localhost:"+Services("queryStwServer"))
  val levenshteinDistanceClient = new HttpClient("localhost:"+Services("levenshteinDistanceService"))

  val name = "TaggerService"

  def apply(port: Int): Int = {
    super.apply(port, name)
  }

  def callPostMethod(path: Array[String], value: String): Future[HttpResponse] = {
    path.head match {
      case "tagText" => postTagText(path.tail, value)
      case _ => Future.value(createHttpResponse("No such method"))
    }
  }

  def callGetMethod(path: Array[String]): Future[HttpResponse] = {
    path.head match {
      case "tagWord" => getTagWord(path(0).toString)
      case _ => Future.value(createHttpResponse("No such method"))
    }
  }

  //attention used .get() here todo improve this!
  def postTagText(args: Array[String], value: String): Future[HttpResponse] = {
    val r = new Promise[HttpResponse]
    tagText(Json.jsonToList(value)) onSuccess { tags => 
      r.setValue(createHttpResponse(Json.listToJson(tags.toList)))
    }
    r
  }

  def getTagWord(value: String): Future[HttpResponse] = {
    //run levenshtein distance algorithm with the word and all stw thesaurus words
    Future.value(createHttpResponse("string"))
  }

  //only use runQueryWithPagination query with ORDER BY

  //Im working with Json strings!
  val query = "PREFIX skos: <http://www.w3.org/2004/02/skos/core#> PREFIX xml: <http://zbw.eu/stw/> SELECT ?q WHERE { ?s ?p ?q FILTER(?p = skos:prefLabel || ?p = skos:altLabel)} ORDER BY ?p"
  val pagination = 1000

  def tagText(text: List[String]): Future[Seq[String]] = {
    if(!(query contains "ORDER BY")) {
     throw new IllegalArgumentException("query needs to use ORDER BY")
    }

    tagTextRec(text, pagination, 0)
  }

  def tagTextRec(text: List[String], pagination: Int, offset: Int): Future[Seq[String]] = {
    if(offset > 4000) {
      return Future.value(Seq(""))
    }

    val test = queryStwClient.post("/runQuery/", query + " LIMIT " + pagination + " OFFSET "+offset) flatMap {labelsJson =>
      val seqFutureString = Json.jsonToValue4Store(labelsJson) map {label => labelForText(label, text)}
      val futureSeqString1 = Future.collect(seqFutureString) 
      val futureSeqString = tagTextRec(text, pagination, offset+1000)
      val seqFuture = Seq(futureSeqString1, futureSeqString)
      Future.collect(seqFuture) map {_.flatten}
    }
    test
  }


  def labelForText(label: String, text: List[String]): Future[String] = {
    levenshteinDistanceClient.post("/labelForText/"+label, Json.listToJson(text))
  }

}

