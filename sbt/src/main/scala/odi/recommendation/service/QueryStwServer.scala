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

/** this service is responsible for the communication with the triple store
  * it is instanciated by the TaggerService
  */
object QueryStwServer extends HttpServer {

  var serverName = "QueryStwServer"

  /** start the service
    * @param port of the service
    * @return the port that the service runs on
    */
  def apply(port: Int): Int = {
    super.apply(port, serverName)
  }


  /** return the descriptor of the preferred label
    *
    * @param a the word that the descriptor should correspond with
    * @return the descriptor if it exists none otherwise
    */
	def findPrefLabel(a: String): Option[String] = {
    val result = getStwPrefLabel(a)
    println(result)
    getDescriptor(result)

	}

  /** return the name of the preferred label that corresponds with the descriptor
    *
    * @param a the name of the preferred label
    * @return the name of the preferred label
    */
  def findTagByPrefLabel(a: String): Option[String] = {
    val result = getStwTag(a)
    useRegexOnTag(result) 
  }

  private

  /** get the descriptor of the preferred label that corresponds with the value
    *
    * @param value the name of the preferred label
    * @return the result of the triple store
    */
  def getStwPrefLabel(value: String): String = {
    val requestValue = value.capitalize
    val sparql = "PREFIX skos: <http://www.w3.org/2004/02/skos/core#> PREFIX xml: <http://zbw.eu/stw/> SELECT * WHERE {?s skos:prefLabel '"+requestValue+"'@de} LIMIT 10"
    println(sparql)
    runQueryOn4Store(sparql)
  }

  /** return the name of the preferred label for the descriptor
    *
    * @param value the descriptor
    * @return the result from the triple store
    */
  def getStwTag(value: String): String = {
    val sparql = "PREFIX skos: <http://www.w3.org/2004/02/skos/core#> PREFIX xml: <http://zbw.eu/stw/> SELECT * WHERE {<http://zbw.eu/stw/descriptor/"+value+"> skos:prefLabel@de ?o} LIMIT 10"
    val result = runQueryOn4Store(sparql)
    result
  }


  /** runs a query on the triple store
    *
    * @param sparql the query that should be executed on the triple store
    * @return the result from the triple store
    */
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

  /** optimize the result from the triple store
    * 
    * @param query the ruselt from the triple store
    * @result the descriptor of the result
    */
  def getDescriptor(query: String): Option[String] = {
    val resultList = Json.jsonToValue4Store(query)
    resultList.map(useRegexOnPrefLabel(_)).headOption getOrElse None
  }

  /** executes a regular expression on a string 
    *
    * @param value the string on which the regularexpression should be executed
    * @return the extracted string
    */
  def useRegexOnPrefLabel(value: String): Option[String] = {
    val regex = """(\d+-\d)""".r
    regex findFirstIn value
  }

  /** exetract a tag from a triple store response with a regular expression
    *
    * @param value the string from which the tag is extracted
    * @return the tag
    */
  def useRegexOnTag(value: String): Option[String] = {
    val regex = """\"value\":\"(\w*)\",\"xml:lang\":\"DE""".r
    for (regex(group) <- regex.findFirstIn(value)) yield group
  }


  /** this method is called by the HttpServer router 
    * and forwards the request to the correct post method
    * @param path the path that the request is send to
    * @param value the value of the post body
    * @return it returns a future http request
    */
  def callPostMethod(path: Array[String], value: String): Future[HttpResponse] = {
    Future.value(path.head match {
      case "runQuery" => postRunQueryOn4Store(path.tail, value)
      case "allPrefLabels" => postAllPrefLabels(value)
      case _ => createHttpResponse("Method "+path.head+" not found")
    })
  }

  /** this method is called by the HttpServer router 
    * and forwards the request to the correct get method
    * @param path the path that the request is send to
    * @return it returns a future http request
    */
  def callGetMethod(path: Array[String]): Future[HttpResponse] = {
    Future.value(path.head match {
      case "prefLabel" => getPrefLabel(path.tail.head)
      case _ => createHttpResponse("Method "+path.head+" not found")
    })
  }

  /** the get method that returns a descriptor for a word
    *
    * @param value the name of the preferred label 
    * @return a http response
    */
  def getPrefLabel(value: String): HttpResponse = {
    val prefLabel = findPrefLabel(value)
    createHttpResponse(prefLabel.getOrElse(""))
  }

  /** return all descriptors for the preferred labels that are included in the json string
    *
    * @param value the json list with the tags
    * @return list of descriptors as a http response
    */
  def postAllPrefLabels(value: String): HttpResponse = {
    val tags = Json.jsonToList(value)
    createHttpResponse(Json.toJson(tags.flatMap(tag => findPrefLabel(tag))))
  }

  /** run a query on the triple store
    *
    * @param args the path of the request
    * @param query the query that should be executed
    * @return the result from the triple store as http response
    */
  def postRunQueryOn4Store(args: Array[String], query: String): HttpResponse = {
    createHttpResponse(runQueryOn4Store(query))
  }
 

}

