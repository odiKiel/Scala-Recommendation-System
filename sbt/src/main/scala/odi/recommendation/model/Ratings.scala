package odi.recommendation
import scala.slick.driver.PostgresDriver.simple._
import scala.slick.jdbc.{ GetResult, StaticQuery => Q }
import Q.interpolation
import Database.threadLocalSession

import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}
import net.liftweb.json.JsonDSL._

 // Definition of the USER_ITEMS table
 //todo prediction!
case class Rating(id: Option[Int] = None, itemId: Int, userId: Int, rating: Int, prediction: Boolean) extends ToJson{
  def toJson = {
    val json = ("id"->id.get)~("itemId"->itemId)~("userId"->userId)~("rating"->rating)~("prediction"->prediction)
    compact(render(json))
  }
}
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


                       
  /*
                       .withSession {

    Ratings.ddl.create

    Ratings.insert(Rating(None, 1, 1, 4))
  }
    */

  def createTable = {
    db.withSession {
      Ratings.ddl.create
    } 

  }

  def byUserId(uid: Int) : List[Rating] = {
    var result:List[Rating] = List[Rating]()

    db withSession {
        // define the query and what we want as result
    	val query = (for {u <-Ratings if u.userId === uid} yield u.id ~ u.itemId ~ u.userId ~ u.rating ~ u.prediction).sortBy(_._2)

    	// map the results to a Bid object
    	val inter = query mapResult {
    	  case(id, itemId, userId, rating, prediction) => Option(Rating(Option(id), itemId, userId, rating, prediction));
    	}

    	// check if there is one in the list and return it, or None otherwise
      if(inter.list != Nil){
        result = inter.list.flatten
      }
      result
    }

    // return the found bid
    result
  }

  def byItemId(iid: Int) : List[Rating] = {
    var result:List[Rating] = List[Rating]()

    db withSession {
        // define the query and what we want as result
    	val query = (for {u <-Ratings if u.itemId === iid} yield u.id ~ u.itemId ~ u.userId ~ u.rating ~ u.prediction).sortBy(_._2)

    	// map the results to a Bid object
    	val inter = query mapResult {
    	  case(id, itemId, userId, rating, prediction) => Option(Rating(Option(id), itemId, userId, rating, prediction));
    	}

    	// check if there is one in the list and return it, or None otherwise
      if(inter.list != Nil){
        result = inter.list.flatten
      }
      result
    }

    // return the found bid
    result
  }


  /*
   find ratings from one user for items that are unknown to another user
   use only ratings that aren't predictions!
   */
  def getUnknownItemsForUserByUser(uid1: Int, uid2: Int): List[Rating] = {
    db withSession {
      val q = Q.query[(Int, Int), (Int, Int, Int, Int, Boolean)]("select * from ratings where user_id = ? AND prediction = FALSE AND item_id NOT IN (select item_id from ratings where user_id = ? and prediction = false) order by rating")
      val inter = q mapResult {
    	  case(id, itemId, userId, rating, prediction) => Rating(Option(id), itemId, userId, rating, prediction);
      }
      inter.list(uid2, uid1)
    }

  }

  def byItemIdUserId(iid: Int, uid: Int) : Option[Rating] = {
    var result:Option[Rating] = None;

    db withSession {
        // define the query and what we want as result
    	val query = for (r <-Ratings if r.userId === uid && r.itemId === iid) yield r.id ~ r.itemId ~ r.userId ~ r.rating ~ r.prediction

    	// map the results to a Bid object
    	val inter = query mapResult {
    	  case(id, itemId, userId, rating, prediction) => Option(Rating(Option(id), itemId, userId, rating, prediction));
    	}

    	// check if there is one in the list and return it, or None otherwise
    	result = inter.list match {
    	  case _ :: tail => inter.first
    	  case Nil => None
    	}

    }

    // return the found bid
    result
  }

 
  def get(rid: Int) : Option[Rating] = {
    var result:Option[Rating] = None;

    db withSession {
        // define the query and what we want as result
    	val query = for (u <-Ratings if u.id === rid) yield u.id ~ u.itemId ~ u.userId ~ u.rating ~ u.prediction

    	// map the results to a Bid object
    	val inter = query mapResult {
    	  case(id, itemId, userId, rating, prediction) => Option(Rating(Option(id), itemId, userId, rating, prediction))
    	}

    	// check if there is one in the list and return it, or None otherwise
    	result = inter.list match {
    	  case _ :: tail => inter.first
    	  case Nil => None
    	}
    }

    // return the found bid
    result
  }

  // return all users with ratings for an item
  def userRatingListByItemId(itemId: Int) : List[(Int, Int)] = {
    db withSession {
      val query = for {
        (user, rating) <- Users leftJoin Ratings on (_.id === _.userId) if rating.itemId === itemId} yield (user.id, rating.rating)
        query.sortBy(_._1).list
      }
    }


  /**
   * Create or update rating if it exists
   */
  def create(rating: Rating): Rating = {
    val ratingOption = byItemIdUserId(rating.itemId, rating.userId)

    if(ratingOption == None) {
      var id: Int = -1;

      // start a db session
      db withSession {
        // create a new bid
        val res = Ratings.noID insert (rating.itemId.intValue, rating.userId.intValue, rating.rating.intValue, rating.prediction.booleanValue)
        // get the autogenerated bid
        val idQuery = Query(SimpleFunction.nullary[Int]("LASTVAL"))
        id = idQuery.list().head
      }
      // create a bid to return
      new Rating(Option(id), rating.itemId, rating.userId, rating.rating, rating.prediction)
    }
    else {
      db withSession {
        val query = for (r <-Ratings if r.userId === rating.userId && r.itemId === rating.itemId) yield r.id ~ r.itemId ~ r.userId ~ r.rating ~ r.prediction
        query.update((ratingOption.get.id.get, rating.itemId, rating.userId, rating.rating, rating.prediction))
      }
      rating
    }
  }

  /**
   * Delete a bid
   */
  def delete(rid: Int) : Option[Rating] = {
    // get the bid we're deleting
    val result = get(rid);

    // delete the bid
    val toDelete = Ratings where (_.id === rid)
    db withSession {
      toDelete.delete
    }

    // return deleted bid
    result
  }

  def deleteByUserId(uid: Int) = {

    val toDelete = Ratings where (_.userId === uid)
    db withSession {
      toDelete.delete
    }

  }

  def deleteByItemId(iid: Int) = {
    val toDelete = Ratings where (_.itemId === iid)
    db withSession {
      toDelete.delete
    }

    // return deleted bid
  }

  def deleteAll = {
    db withSession {
      val q = for { 
        t <- Ratings 
      } yield t 

      q.mutate(_.delete) // deletes rows corresponding to query result 
    }
  }

  def first : Option[Rating] = {
    db withSession {
      val q = Ratings.map{ u => u}.take(1)
      q.list.headOption
    }
  }

  def all : List[Rating] = {
    db withSession {
      val q = Ratings.map{u => u}.sortBy(_.itemId).sortBy(_.userId)
      q.list
    }
  }

  //returns a list of similar items that the user has rated together with the similarity and the rating
  // (ItemId, Rating, Similarity)
  def byUserIdItemIdWithSimilarItem(uid: Int, iid: Int) : List[(Int, Int, Double)] = {

    val result = db withSession {
      val query = for {
        (rating, similarItem) <- Ratings leftJoin SimilarItems on ((r, s) => (r.itemId === s.itemOneId) || (r.itemId === s.itemTwoId)) if(rating.userId === uid && (similarItem.itemOneId === iid || similarItem.itemTwoId === iid))} yield (similarItem.itemOneId, similarItem.itemTwoId, rating.rating, similarItem.similarity)
        //(rating, similarItem) <- Ratings innerJoin SimilarItems on ((r, s) => (r.itemId === s.itemOneId) || (r.itemId === s.itemTwoId)) if(rating.userId === uid && (similarItem.itemOneId === iid || similarItem.itemTwoId === iid))} yield (rating.rating, similarItem.similarity)

      query.sortBy(_._2.desc).take(25).list
    }

  result.map((r) => {
      if(r._1 == iid) (r._2, r._3, r._4)
      else (r._1, r._3, r._4)
  })

  }

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



}
