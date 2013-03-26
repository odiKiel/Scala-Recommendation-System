package odi.recommendation
import org.specs2.mutable._


object SimilarItemsSpec extends Specification {
  "SimilarItems " should {
    TestDatabase.setup()
    val vec = Vector[Int](1,2,3)
    "calculate vector length" in {
      SimilarItems.vectorLength(vec) mustEqual 3.7416575
    }
    "calculate the product of two vectors" in {
      SimilarItems.vectorProduct(vec, vec) mustEqual 14
    }

    "calculate the cosinus similarity of two vectors" in {
      SimilarItems.cosinusSimilarity(vec, vec) mustEqual 1
    }

    "create a RatingVector for an Item and all User that rated the item" in {
      SimilarItems.createRatingVector(Items.get(1).get, Users.getAll().get) mustEqual Vector[Int](5, 3, 4, 3, 1)
    } 

    "calculate the similarity of two items with the ratings of all users that rated this items" in {

      SimilarItems.calculateItemSimilarityUsers(Users.getAll().get.drop(1), Items.get(1).get, Items.get(5).get) mustEqual 0.99
    }

  }

}
