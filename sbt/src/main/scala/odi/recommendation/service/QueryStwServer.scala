package odi.recommendation

import com.twitter.finagle.builder.{Server, ServerBuilder}
import com.twitter.finagle.Service
import com.twitter.util.{Promise, Future}
import org.jboss.netty.handler.codec.http.{HttpResponseStatus, DefaultHttpResponse, DefaultHttpRequest, HttpRequest, HttpResponse, HttpVersion, HttpMethod, HttpHeaders}
import java.io.IOException
import java.net.MalformedURLException
import scala.util.matching.Regex
import uk.co.magus.fourstore.client.Store
import net.liftweb.json._

//will be instanciated by the TaggerService
object QueryStwServer extends HttpServer {

	//val sparql = "SELECT * WHERE { ?s ?p ?o } LIMIT 10"

  //val port = 11500
  //val name = "QueryStwServer"
  var serverName = "QueryStwServer"

  //return the port that the server runs at
  def apply(port: Int): Int = {
    super.apply(port, serverName)
  }


	def findPrefLabel(a: String): Option[String] = {

    val result = getStwPrefLabel(a)
    println(result)
    getDescriptor(result)

	}

  def findTagByPrefLabel(a: String): Option[String] = {
    val result = getStwTag(a)
    useRegexOnTag(result) //.headOption getOrElse None
  }

  private

  def getStwPrefLabel(value: String): String = {
    val requestValue = value.capitalize
    //val sparql = "PREFIX skos: <http://www.w3.org/2004/02/skos/core#> PREFIX xml: <http://zbw.eu/stw/> SELECT * WHERE { {?s skos:prefLabel '"+requestValue+"'@de} UNION{?s skos:altLabel '"+requestValue+"'@de}} LIMIT 10"
    val sparql = "PREFIX skos: <http://www.w3.org/2004/02/skos/core#> PREFIX xml: <http://zbw.eu/stw/> SELECT * WHERE {?s skos:prefLabel '"+requestValue+"'@de} LIMIT 10"
    println(sparql)
    runQueryOn4Store(sparql)
  }

  def getStwTag(value: String): String = {
    val sparql = "PREFIX skos: <http://www.w3.org/2004/02/skos/core#> PREFIX xml: <http://zbw.eu/stw/> SELECT * WHERE {<http://zbw.eu/stw/descriptor/"+value+"> skos:prefLabel@de ?o} LIMIT 10"
    val result = runQueryOn4Store(sparql)
    result
  }


  def runQueryOn4Store(sparql: String): String = {

    var result = ""
		try {
			val store = new Store("http://localhost:8000")
	  	result = store.query(sparql,Store.OutputFormat.JSON)
		} catch {
      case e: MalformedURLException => e.printStackTrace()
      case i: IOException => i.printStackTrace()
		}

    result
  }

  def getDescriptor(query: String): Option[String] = {
    val resultList = Json.jsonToValue4Store(query)
    resultList.map(useRegexOnPrefLabel(_)).headOption getOrElse None
  }

  def useRegexOnPrefLabel(value: String): Option[String] = {
    val regex = """(\d+-\d)""".r
    regex findFirstIn value
  }

  def useRegexOnTag(value: String): Option[String] = {
    val regex = """\"value\":\"(\w*)\",\"xml:lang\":\"DE""".r
    for (regex(group) <- regex.findFirstIn(value)) yield group
  }


  //http Methods and their called methods

  def callPostMethod(path: Array[String], value: String): Future[HttpResponse] = {
    Future.value(path.head match {
      case "runQuery" => postRunQueryOn4Store(path.tail, value)
      case "allPrefLabels" => postAllPrefLabels(value)
      case _ => createHttpResponse("Method "+path.head+" not found")
    })
  }

  def callGetMethod(path: Array[String]): Future[HttpResponse] = {
    Future.value(path.head match {
      case "prefLabel" => getPrefLabel(path.tail.head)
      case _ => createHttpResponse("Method "+path.head+" not found")
    })
  }

  def getPrefLabel(value: String): HttpResponse = {
    val prefLabel = findPrefLabel(value)
    createHttpResponse(prefLabel.getOrElse(""))
  }

  def postAllPrefLabels(value: String): HttpResponse = {
    val tags = Json.jsonToList(value)
    createHttpResponse(Json.toJson(tags.flatMap(tag => findPrefLabel(tag))))
  }

  def postRunQueryOn4Store(args: Array[String], query: String): HttpResponse = {
    createHttpResponse(runQueryOn4Store(query))
  }
 

}

