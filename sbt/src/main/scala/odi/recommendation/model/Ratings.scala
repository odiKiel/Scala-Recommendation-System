package odi.recommendation
import scala.slick.driver.PostgresDriver.simple._
import scala.slick.jdbc.{ GetResult, StaticQuery => Q }
import Q.interpolation
import Database.threadLocalSession

import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}
import net.liftweb.json.JsonDSL._

 /** Definition of the Rating table
   *
   * @param id this is an optional id if a new rating is created None is used
   * @param itemId this is the item id that is connected with this rating
   * @param userId this is the user id that is connected with this rating
   * @param rating this represents the rating for the user item combination
   * @param prediction states if this rating is a prediction or not
   */
case class Rating(id: Option[Int] = None, itemId: Int, userId: Int, rating: Int, prediction: Boolean) extends ToJson{
  def toJson = {
    val json = ("id"->id.get)~("itemId"->itemId)~("userId"->userId)~("rating"->rating)~("prediction"->prediction)
    compact(render(json))
  }
}

/** this object represents the rating table of the database */
object Ratings extends Table[Rating]("ratings") with ModelTrait {
  def id = column[Int]("id", O.PrimaryKey, O.AutoInc) // This is the primary key column
  def itemId = column[Int]("item_id") 
  def userId = column[Int]("user_id")
  def rating = column[Int]("rating")
  def prediction = column[Boolean]("prediction", O.Default(false))
  def * = id.? ~ itemId ~ userId ~ rating ~ prediction <>(Rating, Rating.unapply _)
  def noID = itemId ~ userId ~ rating ~ prediction

  // A reified foreign key relation that can be navigated to create a join
  def user = foreignKey("user_fk", userId, Users)(_.id)
  def item = foreignKey("item_fk", itemId, Items)(_.id)


  /** creates the rating table in the database */
  def createTable = {
    db.withSession {
      Ratings.ddl.create
    } 

  }

  /** returns a list of ratings that correspond with the user id
    * @param uid represents the user id
    * @return a list of ratings that correspond with the user id
    */
  def byUserId(uid: Int) : List[Rating] = {
    var result:List[Rating] = List[Rating]()

    db withSession {
    	val query = (for {u <-Ratings if u.userId === uid} yield u.id ~ u.itemId ~ u.userId ~ u.rating ~ u.prediction).sortBy(_._2)

    	val inter = query mapResult {
    	  case(id, itemId, userId, rating, prediction) => Option(Rating(Option(id), itemId, userId, rating, prediction));
    	}

      if(inter.list != Nil){
        result = inter.list.flatten
      }
      result
    }

    result
  }

  /** returns the ratings for an item id
    *
    * @param iid the item id that is used
    * @return a list of ratings that correspond with the item id
    */
  def byItemId(iid: Int) : List[Rating] = {
    var result:List[Rating] = List[Rating]()

    db withSession {
    	val query = (for {u <-Ratings if u.itemId === iid} yield u.id ~ u.itemId ~ u.userId ~ u.rating ~ u.prediction).sortBy(_._2)

    	val inter = query mapResult {
    	  case(id, itemId, userId, rating, prediction) => Option(Rating(Option(id), itemId, userId, rating, prediction));
    	}

      if(inter.list != Nil){
        result = inter.list.flatten
      }
      result
    }

    result
  }


  /** find ratings from one user for items that are unknown to another user
   *use only ratings that aren't predictions!
   *
   * @param uid1 the user for whom the items of the ratings are unknown
   * @param uid2 the user that rated these items
   * @param the amount of ratings that should be returned
   * @return the list of ratings
   */
  def getUnknownItemsForUserByUser(uid1: Int, uid2: Int, amount: Int): List[Rating] = {
    db withSession {
      val q = Q.query[(Int, Int), (Int, Int, Int, Int, Boolean)]("select * from ratings where user_id = ? AND prediction = FALSE AND item_id NOT IN (select item_id from ratings where user_id = ? and prediction = false) order by rating limit "+amount)
      val inter = q mapResult {
    	  case(id, itemId, userId, rating, prediction) => Rating(Option(id), itemId, userId, rating, prediction);
      }
      inter.list(uid2, uid1)
    }

  }

  /**find ratings from one user for items that are unknown to another user that have the correct tag
   * use only ratings that aren't predictions!
   *
   * @param uid1 the user for whom the items of the ratings are unknown
   * @param uid2 the user that rated these items
   * @param amount of ratings that should be returned
   * @param prefLabel the descriptor that has to correspond with the item
   * @return the list of ratings
   */
  def getUnknownItemsForUserByUserWithTag(uid1: Int, uid2: Int, amount: Int, prefLabel: String): List[Rating] = {
    db withSession {
      val q = Q.query[(Int, String, Int, Int), (Int, Int, Int, Int, Boolean)]("select ratings.id, ratings.item_id, ratings.user_id, ratings.rating, ratings.prediction from ratings INNER JOIN item_tags on ratings.item_id = item_tags.item_id INNER JOIN tags on item_tags.tag_id = tags.id where ratings.user_id = ? AND ratings.prediction = FALSE AND tags.pref_label = ? AND ratings.item_id NOT IN (select item_id from ratings where user_id = ? and prediction = false) order by rating limit ?")
      val inter = q mapResult {
    	  case(id, itemId, userId, rating, prediction) => Rating(Option(id), itemId, userId, rating, prediction);
      }
      inter.list(uid2, prefLabel, uid1, amount)
    }

  }


  /** returns an rating for a specific item user pair
    *
    * @param iid the item id for the user item pair
    * @param uid the user id for the user item pair
    * @return the rating if it exists
    */
  def byItemIdUserId(iid: Int, uid: Int) : Option[Rating] = {
    var result:Option[Rating] = None;

    db withSession {
    	val query = for (r <-Ratings if r.userId === uid && r.itemId === iid) yield r.id ~ r.itemId ~ r.userId ~ r.rating ~ r.prediction

    	val inter = query mapResult {
    	  case(id, itemId, userId, rating, prediction) => Option(Rating(Option(id), itemId, userId, rating, prediction));
    	}

    	result = inter.list match {
    	  case _ :: tail => inter.first
    	  case Nil => None
    	}

    }

    result
  }

 
  /** returns the rating for the rating id
    *
    * @param rid the id for the rating
    * @result the rating if it exists
    */
  def get(rid: Int) : Option[Rating] = {
    var result:Option[Rating] = None;

    db withSession {
    	val query = for (u <-Ratings if u.id === rid) yield u.id ~ u.itemId ~ u.userId ~ u.rating ~ u.prediction

    	val inter = query mapResult {
    	  case(id, itemId, userId, rating, prediction) => Option(Rating(Option(id), itemId, userId, rating, prediction))
    	}

    	result = inter.list match {
    	  case _ :: tail => inter.first
    	  case Nil => None
    	}
    }

    result
  }

  /** return all users with ratings for an item
    *
    * @param itemId the item that the rating list should correspond with
    * @return a list of user rating tupel
    */
  def userRatingListByItemId(itemId: Int) : List[(Int, Int)] = {
    db withSession {
      val query = for {
        (user, rating) <- Users leftJoin Ratings on (_.id === _.userId) if rating.itemId === itemId} yield (user.id, rating.rating)
        query.sortBy(_._1).list
      }
    }


  /** Create or update rating if it exists
    *
    * @param rating the rating that should be created or updated
    * @return the rating that is created or updated
    */
  def create(rating: Rating): Rating = {
    //load rating if it exists
    val ratingOption = byItemIdUserId(rating.itemId, rating.userId)

    if(ratingOption == None) {
      //create new rating if it does not exist
      var id: Int = -1;

      db withSession {
        val res = Ratings.noID insert (rating.itemId.intValue, rating.userId.intValue, rating.rating.intValue, rating.prediction.booleanValue)
        val idQuery = Query(SimpleFunction.nullary[Int]("LASTVAL"))
        id = idQuery.list().head
      }
      new Rating(Option(id), rating.itemId, rating.userId, rating.rating, rating.prediction)
    }
    else {
      //update rating if it exists
      db withSession {
        val query = for (r <-Ratings if r.userId === rating.userId && r.itemId === rating.itemId) yield r.id ~ r.itemId ~ r.userId ~ r.rating ~ r.prediction
        query.update((ratingOption.get.id.get, rating.itemId, rating.userId, rating.rating, rating.prediction))
      }
      rating
    }
  }

  /** Delete a rating
    * 
    * @param rid the rating id for the rating that should be deleted
    * @result the rating that is deleted
    */
  def delete(rid: Int) : Option[Rating] = {
    val result = get(rid);

    val toDelete = Ratings where (_.id === rid)
    db withSession {
      toDelete.delete
    }

    result
  }

  /** delete a rating by user id
    *
    * @param uid the user id that should be removed from all ratings
    */
  def deleteByUserId(uid: Int) = {

    val toDelete = Ratings where (_.userId === uid)
    db withSession {
      toDelete.delete
    }

  }

  /** delete a rating by item id
    *
    * @param iid the item id that should be removed from all ratings
    */
  def deleteByItemId(iid: Int) = {
    val toDelete = Ratings where (_.itemId === iid)
    db withSession {
      toDelete.delete
    }

    // return deleted bid
  }

  /** delete all ratings */
  def deleteAll = {
    db withSession {
      val q = for { 
        t <- Ratings 
      } yield t 

      q.mutate(_.delete) // deletes rows corresponding to query result 
    }
  }

  /** return the first rating */
  def first : Option[Rating] = {
    db withSession {
      val q = Ratings.map{ u => u}.take(1)
      q.list.headOption
    }
  }

  /** return all ratings
    *
    * @return a list of all ratings
    */
  def all : List[Rating] = {
    db withSession {
      val q = Ratings.map{u => u}.sortBy(_.itemId).sortBy(_.userId)
      q.list
    }
  }

  /** returns a list of similar items that the user has rated together with the similarity and the rating
    *
    * @param uid the user id for the user item pair
    * @param iid the item id for the user item pair
    * @return a list of (ItemId, Rating, Similarity)
    */

  def byUserIdItemIdWithSimilarItem(uid: Int, iid: Int) : List[(Int, Int, Double)] = {

    val result = db withSession {
      val query = for {
        (rating, similarItem) <- Ratings leftJoin SimilarItems on ((r, s) => (r.itemId === s.itemOneId) || (r.itemId === s.itemTwoId)) if(rating.userId === uid && (similarItem.itemOneId === iid || similarItem.itemTwoId === iid))} yield (similarItem.itemOneId, similarItem.itemTwoId, rating.rating, similarItem.similarity)

      query.sortBy(_._2.desc).take(25).list
    }

  result.map((r) => {
      if(r._1 == iid) (r._2, r._3, r._4)
      else (r._1, r._3, r._4)
  })

  }

  /** returns a list of similar users that the user has rated together with the similarity 
    */
  def byUserIdItemIdWithSimilarUserAverageRating(userId: Int, itemId: Int) : List[(Int, Int, Double, Double)] = {

    db withSession {
      val query = for {
        ((rating, similarUser), user) <- Ratings innerJoin SimilarUsers on ((r, s) => (r.userId === s.userOneId) || (r.userId === s.userTwoId)) innerJoin Users on((r,s) => (r._1.userId === s.id)) if(rating.itemId === itemId && (similarUser.userOneId === userId || similarUser.userTwoId === itemId))} yield (user.id, rating.rating, similarUser.similarity, user.averageRating)

      query.sortBy(_._3.desc).take(25).list
    }
  }


  //return for a user all similar users that rated a specific item with the averageRating
  /*
  def byUserIdItemIdWithSimilarUserAverageRating(userId: Int, itemId: Int): List[(Int, Int, Double, Double)] = {
    db withSession {

      val query = for {
        r <- Ratings if r.itemId === itemId
        s <- SimilarUsers if((r.userId === s.userOneId && s.userTwoId === userId) || (r.userId === s.userTwoId && s.userOneId === userId))
        u <- Users if r.userId === u.id
      } yield (u.id, r.rating, s.similarity, u.averageRating)
      
      query.sortBy(_._3.desc).take(25).list
    }

  }
  */

  //returns all user ratings for an item id if the user has not rated the item it returns null 
  def allUserRatingsForItemId(itemId: Int): List[(Int, Int, Option[Int])] = {
    db withSession{
      val query = for {
        //innerJoin on true hack for postgreSQL slick bug
        ((user, item), rating) <- Users innerJoin Items on ((a, b) => a.id === a.id) leftJoin Ratings on ((a, b) => a._1.id === b.userId && a._2.id === b.itemId) if(item.id === itemId)
      }yield (user.id, item.id, rating.rating.?)
      query.sortBy(_._1).list
    }
  }

  //returns all item ratings for an user id if the user has not rated the item it returns null 
  def allItemRatingsForUserId(userId: Int): List[(Int, Int, Option[Int], Double)] = {
    db withSession{
      val query = for {
        //innerJoin on true hack for postgreSQL slick bug
        ((user, item), rating) <- Users innerJoin Items on ((a, b) => a.id === a.id) leftJoin Ratings on ((a, b) => a._1.id === b.userId && a._2.id === b.itemId) if(user.id === userId)
      }yield (user.id, item.id, rating.rating.?, item.averageRating)
      query.sortBy(_._2).list
    }
  }

  /** calculate Rating 
    * 
    * with the time a user spends on the page and the time a user scrolls the page
    */
  def calculateRatingByTimes(item: Item, userId: Int, timeSpend: Double, timeScroll: Double, userInteraction: Boolean) = {
    if(userInteraction) { //if userinteraction this user is highly interested
      Ratings.create(Rating(None, item.id.get, userId, 5, false))
    }
    else {
      if(item.truncatedAmount > 10) {
        val timeSpendPercentage = timeSpend / item.truncatedTimeSpend.toDouble
        val timeScrollPercentage = timeScroll / item.truncatedTimeScroll.toDouble
        val spendRating = (timeSpendPercentage) match {
          case x if x < 0.765 => 1
          case x if x >= 0.765 && x < 0.9439 => 2
          case x if x >= 0.9439 && x < 1.0965 => 3
          case x if x >= 1.0965 => 4
        }
        val scrollRating = (timeScrollPercentage) match {
          case x if x < 0.69 => 1
          case x if x >= 0.69 && x < 0.86 => 2
          case x if x >= 0.86 && x < 1.08 => 3
          case x if x >= 1.08 && x < 1.29 => 4
          case x if x >= 1.29 => 5
        }

        val rating = ((spendRating + scrollRating) / 2.0)
        val totalRating = rating.round.toInt
        Ratings.create(Rating(None, item.id.get, userId, totalRating, false))
      }
    }
      
  }



}
