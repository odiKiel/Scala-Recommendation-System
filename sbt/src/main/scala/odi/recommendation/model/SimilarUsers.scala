package odi.recommendation
import scala.slick.driver.PostgresDriver.simple._
import Database.threadLocalSession

import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}
import net.liftweb.json.JsonDSL._
import org.apache.commons.math3.linear._

/** this class represents an entry in the SimilarUser table 
  * 
  * @param id the id of the SimilarUser
  * @param userOneId the id of the first User
  * @param userTwoId the id of the second user
  * @param similarity a double value that represents the similarity
  */
case class SimilarUser(id: Option[Int] = None, userOneId: Int, userTwoId: Int, similarity: Double) extends ToJson with ModelTrait{

  /** creates a Json string for this object */
  def toJson = {
    val json = ("id"->id.get)~("userOneId"->userOneId)~("userTwoId"->userTwoId)~("similarity"->similarity)
    compact(render(json))
  }

  /** returns the other user id with the similarity value 
    *
    * @param userId the user id that is known
    * @return tuple of the unknown userId with the similarity value
    */
  def similarityByUserId(userId: Int): Option[(Int, Double)] = {
    if(userId == userOneId) {
      Some((userTwoId, similarity))
    }
    else {
      if(userId == userTwoId) {
        Some((userOneId, similarity))
      }
      else {
        None
      }
    }
  }
}

/** this object represents the SimilarUser table of the database */
object SimilarUsers extends Table[SimilarUser]("similar_users") with VectorCalculation with ModelTrait {
  def id = column[Int]("id", O.PrimaryKey, O.AutoInc) // This is the primary key column
  def userOneId = column[Int]("user_one_id") 
  def userTwoId = column[Int]("user_two_id")
  def similarity = column[Double]("similarity")
  def * = id.? ~ userOneId ~ userTwoId ~ similarity <>(SimilarUser, SimilarUser.unapply _)
  def noID = userOneId ~ userTwoId ~ similarity

  // A reified foreign key relation that can be navigated to create a join
  def userOne = foreignKey("user_one_fk", userOneId, Users)(_.id)
  def userTwo = foreignKey("user_two_fk", userTwoId, Users)(_.id)


  /** creates the table in the database */
  def createTable = {
    db.withSession {
      SimilarUsers.ddl.create
    } 

  }
 
  /** returns the SimilarUser entry for the id
    *
    * @param sid the SimilarUser id for the SimilarUser
    * @return the SimilarUser if it exists otherwise None
    */
  def get(sid: Int) : Option[SimilarUser] = {
    var result:Option[SimilarUser] = None;

    db withSession {
        // define the query and what we want as result
    	val query = for (s <-SimilarUsers if s.id === sid) yield s.id ~ s.userOneId ~ s.userTwoId ~ s.similarity 

    	
    	val inter = query mapResult {
    	  case(id, userOneId, userTwoId, similarity) => Option(SimilarUser(Option(id), userOneId, userTwoId, similarity))
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

  /** returns the first element of the table */
  def first : Option[SimilarUser] = {
    db withSession {
      val q = SimilarUsers.map{ u => u}.take(1)
      q.list.headOption
    }
  }

  /** returns a list of SimilarUsers that correspond with the user id
    *
    * @param userId the user that the list should correspond with
    * @param amount the amount of list entries that should be returned
    * @return a list of similar users that correspond with the user id
    */
  def byUserId(userId: Int, amount: Int) : List[SimilarUser] = {

    db withSession {
    	val query = for (s <-SimilarUsers if s.userOneId === userId || s.userTwoId === userId) yield s.id ~ s.userOneId ~ s.userTwoId ~ s.similarity 

    	
    	val inter = query.sortBy(_._4).take(amount) mapResult {
    	  case(id, userOneId, userTwoId, similarity) => SimilarUser(Option(id), userOneId, userTwoId, similarity)
    	}

    	inter.list 
    }

  }


  /** returns a SimilarUser entry for two users if it exists
    *
    * @param user1Id the id of the first user
    * @param user2Id the id of the second user
    * @return the SimilarUser entry if it exists
    */
  def getByUserUser(user1Id: Int, user2Id: Int) : Option[SimilarUser] = {
    var result:Option[SimilarUser] = None;

    db withSession {
    	val query = for (s <-SimilarUsers if s.userOneId === user1Id && s.userTwoId === user2Id || s.userOneId === user2Id && s.userTwoId === user1Id) yield s.id ~ s.userOneId ~ s.userTwoId ~ s.similarity 

    	
    	val inter = query mapResult {
    	  case(id, userOneId, userTwoId, similarity) => Option(SimilarUser(Option(id), userOneId, userTwoId, similarity))
    	}

    	result = inter.list match {
    	  case _ :: tail => inter.first
    	  case Nil => None
    	}
    }

    result
  }

  /** return all similar users */
  def all : List[SimilarUser] = {
    db withSession {
      val q = SimilarUsers.map{u => u}.sortBy(_.userTwoId).sortBy(_.userOneId)
      q.list
    }
  }


  /** Create similarUser if it doesnt exist or update if it has a new value */
  def createOrUpdate(similarUser: SimilarUser): SimilarUser = {
    val oldSimilarUser: Option[SimilarUser] = getByUserUser(similarUser.userOneId, similarUser.userTwoId)
    //check if entry exists
    if(oldSimilarUser == None ) {
      // no entry exists create new one
      var id: Int = -1;

      db withSession {
        val res = SimilarUsers.noID insert (similarUser.userOneId.intValue, similarUser.userTwoId.intValue, similarUser.similarity.floatValue)
        val idQuery = Query(SimpleFunction.nullary[Int]("LASTVAL"))
        id = idQuery.list().head
      }
      new SimilarUser(Option(id), similarUser.userOneId, similarUser.userTwoId, similarUser.similarity)
    }
    else {
      // entry exists check if new entry must be created
      if(Math.abs(oldSimilarUser.get.similarity - similarUser.similarity) > 0.01) {
        db withSession {
          val query = for (s <-SimilarUsers if s.userOneId === similarUser.userOneId && s.userTwoId === similarUser.userTwoId) yield s.userOneId ~ s.userTwoId ~ s.similarity
          query.update((similarUser.userOneId, similarUser.userTwoId, similarUser.similarity))
        }
        similarUser
      }
      else {
        oldSimilarUser.get
      }
    }
  }

  /** Delete a similar user entry */
  def delete(sid: Int) : Option[SimilarUser] = {
    val result = get(sid);

    val toDelete = SimilarUsers where (_.id === sid)
    db withSession {
      toDelete.delete
    }

    result
  }


  /** calculates the similarity between users
    *
    * @param a matrix that represents the users
    */
  def calculateSimilarity(similarMatrix: RealMatrix) = {
    val allUsers = Users.all
    val similarUsers = collection.mutable.ArrayBuffer[SimilarUser]()
    for(i <- (0 until allUsers.length-1);
        j <- (i+1 until allUsers.length)) {
        similarUsers += (
          SimilarUser(
            None, 
            allUsers(i).id.get, 
            allUsers(j).id.get, 
            similarMatrix.getRowVector(i).cosine(similarMatrix.getRowVector(j))
          )
        )
      println("done with user "+i+" user "+j)
    }
    createAll(similarUsers)
  }


  /** creates all similar users that are in the array
    *
    * @param similarUsers an array of similarUsers
    */
  def createAll(similarUsers: collection.mutable.ArrayBuffer[SimilarUser]) = {
    db withSession {
      similarUsers.foreach{(similarUser: SimilarUser) => {
        SimilarUsers.noID insert (similarUser.userOneId, similarUser.userTwoId, similarUser.similarity)
      }}
    }

  }


  /** deletes all similar users */
  def deleteAll = {
    db withSession {
      val q = for { 
        t <- SimilarUsers 
      } yield t 

      q.mutate(_.delete) // deletes rows corresponding to query result 
    }
  }

  /** deletes all similar users that correspond with the user id */
  def deleteByUserId(iid: Int) = {
    db withSession {
      val q = for (t <- SimilarUsers if t.userOneId === iid || t.userTwoId === iid) yield t 

      q.mutate(_.delete) // deletes rows corresponding to query result 
    }


  }

}
