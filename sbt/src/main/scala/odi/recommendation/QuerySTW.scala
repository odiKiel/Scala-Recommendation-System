package odi.recommendation

import java.io.IOException
import java.net.MalformedURLException
import scala.util.matching.Regex
import uk.co.magus.fourstore.client.Store
import net.liftweb.json._

object QueryStw {

	//val sparql = "SELECT * WHERE { ?s ?p ?o } LIMIT 10"


	def findDescriptor(a: String): Option[String] = {

    val result = runQueryOn4Store(a)
    getDescriptor(result)

	}

  private

  def runQueryOn4Store(value: String): String = {
    val sparql = "PREFIX skos: <http://www.w3.org/2004/02/skos/core#> PREFIX xml: <http://zbw.eu/stw/> SELECT * WHERE { ?s skos:prefLabel '"+value+"'@de} LIMIT 10"

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
    val json = parse(query)
    val resultList = for {JField("value", JString(value)) <- json} yield value
    if(resultList.isEmpty) {
      return None
    }
    resultList.map(useRegex(_)).head
  }

  def useRegex(value: String): Option[String] = {
    val regex = """(\d+-\d)""".r
    regex findFirstIn value
  }
  

}

