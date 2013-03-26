package odi.recommendation

import org.specs2.mutable._

object QueryStwServerSpec extends Specification {
  "QueryStw findDescriptor" should {
    val queryStwServer = QueryStwServer(11500, "QueryStw")
    val queryStwClient = new HttpClient("localhost:"+queryStwServer)
    "return a prefLabel for a string if it exist" in {
      queryStwClient.get("/prefLabel/Hanf").get() mustEqual "19017-2"
    }
    "return None if it doesn't exist" in {
      queryStwClient.get("/prefLabel/NA").get() mustEqual ""
    }

//  "return every word from stw" in {
//    QueryStw.getAllStw.length mustEqual 32683
//  }
  }
}
