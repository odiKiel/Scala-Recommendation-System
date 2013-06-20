package odi.recommendation
import scala.slick.driver.PostgresDriver.simple._
import Database.threadLocalSession
import scala.slick.driver.BasicInvokerComponent

import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}
import net.liftweb.json.JsonDSL._


/** Definition of the USERS table
  *
  * @param id the id of the user entry
  * @param the name of the user (optional)
  * @param averageRating the average rating of the user
  * @param qUserId the user id of the question and answer system
  */
case class User(id: Option[Int] = None, name: String, averageRating: Double, qUserId: Int) extends ToJson with ModelTrait{

  /** returns a json interpretation of this user */
  def toJson = {
    val json = ("id"->id.get)~("name"->name)~("qUserId"->qUserId)
    compact(render(json))
  }

  /** calculates the average rating for this user */
  def calculateAverageRating = {
    val ratings = Ratings.byUserId(id.get)
    val averageRating = if(ratings.length == 0) {
      3.0 //if no ratings for this user use the scale middle
    }
    else {
      ratings.map(_.rating).sum / ratings.length.toDouble
    }

    db withSession {
      val query = for (u <-Users if u.id === id.get ) yield u.averageRating 
      query.update(averageRating)
    }
  }

}

  /** this object represents the user table of the database */
object Users extends Table[User]("users") with ModelTrait{
  def id = column[Int]("id", O.PrimaryKey, O.AutoInc) // This is the primary key column
  def name = column[String]("name")
  def averageRating = column[Double]("average_rating")
  def qUserId = column[Int]("q_user_id")
  def * = id.? ~ name ~ averageRating ~ qUserId <> (User, User.unapply _)
  def noID = name ~ averageRating ~ qUserId


  /** create a new user table in the database */
  def createTable = {
    db.withSession {
      Users.ddl.create
    } 
  }

  /** return the user for the id
    *
    * @param uid the id of the user that should be returned
    * @return the user if it exists otherwise none
    */
  def get(uid: Int) : Option[User] = {
    var result:Option[User] = None;

    db withSession {
    	val query = for (u <-Users if u.id === uid) yield u.id ~ u.name ~ u.averageRating ~ u.qUserId

    	val inter = query mapResult {
    	  case(id, name, averageRating, qUserId) => Option(User(Option(id), name, averageRating, qUserId))
    	}

    	result = inter.list match {
    	  case _ :: tail => inter.first
    	  case Nil => None
    	}
      result
    }
  }


  /** return the user by its corresponding question and answer user id
    *
    * @param qUserId the user id of the question and answer system
    * @return the user if it exists otherwise none
    */
  def byQUserId(qUserId: Int) : Option[User] = {
    var result:Option[User] = None;

    db withSession {
    	val query = for (u <-Users if u.qUserId === qUserId) yield u.id ~ u.name ~ u.averageRating ~ u.qUserId

    	val inter = query mapResult {
    	  case(id, name, averageRating, qUserId) => Option(User(Option(id), name, averageRating, qUserId))
    	}

    	result = inter.list match {
    	  case _ :: tail => inter.first
    	  case Nil => None
    	}
      result
    }
  }


  /** return all users that rated a specific item
    *
    * @param itemId the id of the item
    * @return a list of user ids
    */
  def userIdsForItemId(itemId: Int) : List[Int] = {
    db withSession {
    	val query = for (r <-Ratings if r.itemId === itemId;
                       u <- Users if u.id === r.userId) yield u.id 

    	val inter = query mapResult {
    	  case(id) => id
    	}

      inter.list
    }
  }

  /** returns a list of users that rated an item together with tha rating
    *
    * @param itemId the id of the item
    * @return a list of userid, rating tuples
    */
  def userIdsForItemIdWithRating(itemId: Int) : List[(Int, Int)] = {
    db withSession {
    	val query = for (r <-Ratings if r.itemId === itemId;
                       u <- Users if u.id === r.userId) yield u.id ~ r.rating 

    	val inter = query mapResult {
    	  case(id, rating) => (id, rating)
    	}

      inter.list
    }
  }

  /** returns a list of user, rating tupels for an item the rating is normalized by the average rating of that user
    *
    * @param itemId the id of the item
    * @return a list of user id normalized rating tuple
    */
  def userIdsForItemIdWithRatingNormalized(itemId: Int): List[(Int, Double)] = {
    db withSession {
    	val query = for (r <-Ratings if r.itemId === itemId;
                       u <- Users if u.id === r.userId) yield u.id ~ r.rating ~ u.averageRating

    	val inter = query mapResult {
        case(id, rating, averageRating) => (id, rating-averageRating)
    	}

      inter.list
    }
  }

  /** Create a new User */
  def create(user: User): User = {
    var id: Int = -1;

    println(user.name+" "+user.averageRating+" "+user.qUserId)

    db withSession {
      val res = Users.noID insert (user.name, 0.0, user.qUserId)
      val idQuery = Query(SimpleFunction.nullary[Int]("LASTVAL"))
      id = idQuery.list().head
    }
    new User(Option(id), user.name, 0.0, user.qUserId)
  }


  /** Delete a user
    *
    * @param uid the id of the user that should be deleted
    */
  def delete(uid: Int) = {
    Ratings.deleteByUserId(uid)
    SimilarUsers.deleteByUserId(uid)

    val toDelete = Users where (_.id === uid)
    db withSession {
      toDelete.delete
    }

  }

  /** return the first user of the database
    *
    * return user if it exists otherwise none
    */
  def first : Option[User] = {
    db withSession {
      val q = Users.map{ u => u}.sortBy(_.id).take(1)
      q.list.headOption
    }
  }



  /** return all users of the database
    *
    * @result a list of all users
    */
  def all : List[User] = {
    db withSession {
      val q = Users.map({u => u}).sortBy(_.id)
      q.list
    }
  }


  /** delete all users of the database */
  def deleteAll = {
    db withSession {
      val q = for { 
        t <- Users 
      } yield t 

      q.mutate(_.delete) // deletes rows corresponding to query result 
    }
  }

  /** calculate the average rating of all users */
  def calculateAverageRating = {
    val time = System.nanoTime
    Users.all.foreach((user: User) => user.calculateAverageRating)
    println("time: "+(System.nanoTime-time))
  }

}
