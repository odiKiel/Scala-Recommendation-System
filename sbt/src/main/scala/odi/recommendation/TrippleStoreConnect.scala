package odi.recommendation

import java.io.IOException
import java.net.MalformedURLException
import uk.co.magus.fourstore.client.Store

object Query {

	val sparql = "SELECT * WHERE { ?s ?p ?o } LIMIT 10"

	def apply() = {

		try {
			println("Start")
			val store = new Store("http://localhost:8000")
			//simple query
			val response1 = store.query(sparql)
			println(response1)
			//specifying outputformat
			val response2 = store.query(sparql,Store.OutputFormat.JSON)
			println(response2)
			//specifying softlimit and default output format
			val response3 = store.query(sparql,5)
			println(response3)
			//specifying outputformat and soft limit
			val response4 = store.query(sparql,Store.OutputFormat.TAB_SEPARATED, 1)
			println(response4)
			println("Done")
		} catch {
      case e: MalformedURLException => e.printStackTrace()
      case i: IOException => i.printStackTrace()
		}
	}
}

