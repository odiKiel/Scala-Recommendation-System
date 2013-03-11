package odi.recommendation

import java.io.IOException
import java.net.MalformedURLException
import scala.util.matching.Regex
import uk.co.magus.fourstore.client.Store
import net.liftweb.json._

object QueryStw {

	//val sparql = "SELECT * WHERE { ?s ?p ?o } LIMIT 10"


	def findDescriptor(a: String): Option[String] = {

    val result = getStwPrefLabel(a)
    getDescriptor(result)

	}

  //optimize this
  //return a list of futures so we can work on the list before it returns everything
  //use flatmap afterwards to call function getTag on each list entry
  def getAllStw(): List[String] = {

    val query = "PREFIX skos: <http://www.w3.org/2004/02/skos/core#> PREFIX xml: <http://zbw.eu/stw/> SELECT ?q WHERE { ?s ?p ?q FILTER(?p = skos:prefLabel || ?p = skos:altLabel)} ORDER BY ?p"
    runQueryWithPagination(query, 1000)
  }

  private

  def getStwPrefLabel(value: String): String = {
    val sparql = "PREFIX skos: <http://www.w3.org/2004/02/skos/core#> PREFIX xml: <http://zbw.eu/stw/> SELECT * WHERE { {?s skos:prefLabel '"+value+"'@de} UNION{?s skos:prefLabel '"+value+"'@de}} LIMIT 10"
    runQueryOn4Store(sparql)
  }

  //only use runQueryWithPagination query with ORDER BY
  def runQueryWithPagination(query: String, pagination: Int): List[String] = {
    if(!(query contains "ORDER BY")) {
     throw new IllegalArgumentException("query needs to use ORDER BY")
    }
    var valueList = List("")
    var length = pagination
    var offset = 0
    while(length == pagination) {
      val string = runQueryOn4Store(query + " LIMIT " + pagination + " OFFSET "+offset)
      val currentList = getValueFromJson(string)
      length = currentList.length
      offset += length
      valueList = valueList ::: currentList
    }
    valueList
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

  def getValueFromJson(query: String): List[String] = {
    val json = parse(query)
    for {JField("value", JString(value)) <- json} yield value
  }

  def getDescriptor(query: String): Option[String] = {
    val resultList = getValueFromJson(query)
    resultList.map(useRegex(_)).headOption getOrElse None
  }

  def useRegex(value: String): Option[String] = {
    val regex = """(\d+-\d)""".r
    regex findFirstIn value
  }
  

}

