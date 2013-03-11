package odi.recommendation

import org.specs2.mutable._

object QueryStwSpec extends Specification {
  "QueryStw findDescriptor" should {
    "return a descriptor for a string if it exist" in {
      QueryStw.findDescriptor("Hanf") mustEqual Some("19017-2")
    }
    "return None if it doesn't exist" in {
      QueryStw.findDescriptor("N/A") mustEqual None
    }

    "return every word from stw" in {
      QueryStw.getAllStw.length mustEqual 32683
    }
  }
}
