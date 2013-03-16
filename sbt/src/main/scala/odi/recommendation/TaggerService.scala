package odi.recommendation
import org.jboss.netty.handler.codec.http.{HttpResponse}
import com.twitter.finagle.builder.Server
import com.twitter.util.{Promise, Future}
import net.liftweb.json._


object TaggerService extends HttpServer {
//  val port = 11000
//  val name = "TaggerService"
  val queryStwClient = new HttpClient("localhost:"+Services("queryStwServer"))
  val name = "TaggerService"
  def apply(port: Int): Int = {
    super.apply(port, name)
  }

  def callPostMethod(path: Array[String], value: String): HttpResponse = {
    path.head match {
      case "tagText" => postTagText(path.tail, value)
    }
  }

  def callGetMethod(path: Array[String]): HttpResponse = {
    path.head match {
      case "tagWord" => getTagWord(path(0).toString)
    }
  }

  def postTagText(args: Array[String], value: String): HttpResponse = {
    queryStwClient.get("/prefLabel/"+value)
    createHttpResponse("json")
  }

  def getTagWord(value: String): HttpResponse = {
    //run levenshtein distance algorithm with the word and all stw thesaurus words
    createHttpResponse("string")
  }

  //only use runQueryWithPagination query with ORDER BY
  //todo: remove while onSuccess if labelList.length() < pagination done else start new request

  //Im working with Json strings!
  val query = "PREFIX skos: <http://www.w3.org/2004/02/skos/core#> PREFIX xml: <http://zbw.eu/stw/> SELECT ?q WHERE { ?s ?p ?q FILTER(?p = skos:prefLabel || ?p = skos:altLabel)} ORDER BY ?p"
  def tagText(pagination: Int, text: List[String]): Future[Future[Seq[String]]] = {
    if(!(query contains "ORDER BY")) {
     throw new IllegalArgumentException("query needs to use ORDER BY")
    }

    /*
    var seqFutureLabels: List[Future[Seq[String]]] = null
    val futureLabels = Promise[Future[Seq[String]]]

    queryStwClient.post("/runQuery/", query + " LIMIT " + pagination + " OFFSET "+0) onSuccess { labelsJson => 
      val labels = getValueFromJson(labelsJson)
      if(labels.length == pagination) {
        seqFutureLabels = seqFutureLabels :+ tagTextRec(text, pagination, labels.length)
      }
      val futSeqString = Future.collect(labels.map( label => labelsForText(label, text))).map(_.flatten)
      seqFutureLabels = seqFutureLabels :+ futSeqString
      futureLabels.setValue(Future.collect(seqFutureLabels).map(_.flatten))
    } onFailure { exc => 
      futureLabels.setException(exc)
    }



    futureLabels
    */
    tagTextRec(text, pagination, 0)
  }
  /*
   tagTextRec => Future[Future[Seq[String]]] 
   ganz normal rekursion abbruchbedingung wenn offset > stw size return
   */

  def tagTextRec(text: List[String], pagination: Int, offset: Int): Future[Future[Seq[String]]] = {
    val futureLabels = Promise[Future[Seq[String]]]
    var seqFutureLabels: List[Future[Seq[String]]] = null
    queryStwClient.post("/runQuery/", query + " LIMIT " + pagination + " OFFSET "+offset) onSuccess { labelsJson => 
      val labels = getValueFromJson(labelsJson)
      if(labels.length == pagination) {
        seqFutureLabels = seqFutureLabels :+ tagTextRec(text, pagination, labels.length + offset)
      }
      val futSeqString = Future.collect(labels.map( label => labelsForText(label, text))).map(_.flatten)
      seqFutureLabels = seqFutureLabels :+ futSeqString
   //   futureLabels.setValue(Future.collect(seqFutureLabels))
      futureLabels.setValue(Future.collect(seqFutureLabels).map(_.flatten))
    } onFailure { exc => 
      futureLabels.setException(exc)
    }
    futureLabels
  }

  def getValueFromJson(query: String): List[String] = {
    val json = parse(query)
    for {JField("value", JString(value)) <- json} yield value
  }


  //todo: should be a Service
  def labelsForText(label: String, text: List[String]): Future[Seq[String]] = {
    Promise[Seq[String]]
  }

}

