package odi.recommendation
import org.specs2.mutable._


object SimilarItemsSpec extends Specification {
  "SimilarItems " should {
    TestDatabase.setup
    val vec = Vector[Double](1.0,2.0,3.0)
    "calculate vector length" in {
      SimilarItems.vectorLength(vec) mustEqual 3.7416573867739413
    }
    "calculate the product of two vectors" in {
      SimilarItems.vectorProduct(vec, vec) mustEqual 14
    }

    "calculate the cosinus similarity of two vectors" in {
      //should be 1 but because of floating point numbers
      SimilarItems.cosinusSimilarity(vec, vec) mustEqual 1.0
    }

//  "create a RatingVector for an Item and all User that rated the item" in {
//    SimilarItems.createRatingVector(Items.first.get, Users.all) mustEqual Vector[Int](5, 3, 4, 3, 1)
//  } 

//  "calculate the similarity of two items with the ratings of all users that rated this items" in {

//    SimilarItems.calculateItemSimilarityUsers(Users.all.drop(1), Items.first.get, Items.all.last) mustEqual 0.9941002434954168
//  }

  }

}
