package odi.recommendation
import scala.slick.driver.PostgresDriver.simple._
import Database.threadLocalSession
import scala.slick.driver.BasicInvokerComponent

import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}
import net.liftweb.json.JsonDSL._
// Definition of the USERS table
case class User(id: Option[Int] = None, name: String) extends ToJson {
  def toJson = {
    val json = ("id"->id.get)~("name"->name)
    compact(render(json))
  }

}
object Users extends Table[User]("users") {
  def id = column[Int]("id", O.PrimaryKey, O.AutoInc) // This is the primary key column
  def name = column[String]("name")
  // Every table needs a * projection with the same type as the table's type parameter
  def * = id.? ~ name <>(User, User.unapply _)
  def noID = name 

  lazy val db = Database.forURL("jdbc:postgresql://localhost/recommendation",
                         driver="org.postgresql.Driver",
                         user="oliver_diestel",
                         password="")
  def createTable = {
    db.withSession {
      Users.ddl.create
    } 
  }

  def get(uid: Int) : Option[User] = {
    var result:Option[User] = None;

    db withSession {
        // define the query and what we want as result
    	val query = for (u <-Users if u.id === uid) yield u.id ~ u.name 

    	// map the results to a Bid object
    	val inter = query mapResult {
    	  case(id, name) => Option(User(Option(id), name))
    	}

    	// check if there is one in the list and return it, or None otherwise
    	result = inter.list match {
    	  case _ :: tail => inter.first
    	  case Nil => None
    	}
      result
    }
  }

  def usersForItem(item: Item) : List[User] = {
    var result:List[User] = List[User]()

    db withSession {
        // define the query and what we want as result
    	val query = for (r <-Ratings if r.itemId === item.id;
                       u <- Users if u.id === r.userId) yield u.id ~ u.name

    	// map the results to a Bid object
    	val inter = query mapResult {
    	  case(id, name) => Option(User(Option(id), name))
    	}

    	// check if there is one in the list and return it, or None otherwise
      if(inter.list.length > 0) {
        result = inter.list.flatten
      }
    }
    result
  }


  /**
   * Create a bid using scala query. This will always create a new bid
   */
  def create(user: User): User = {
    var id: Int = -1;

    // start a db session
    db withSession {
      // create a new bid
      val res = Users.noID insert (user.name)
      // get the autogenerated bid
      val idQuery = Query(SimpleFunction.nullary[Int]("LASTVAL"))
      id = idQuery.list().head
    }
    // create a bid to return
    new User(Option(id), user.name)
  }


  /**
   * Delete a bid
   */
  def delete(uid: Int) = {
    Ratings.deleteByUserId(uid)
    SimilarUsers.deleteByUserId(uid)

    val toDelete = Users where (_.id === uid)
    db withSession {
      toDelete.delete
    }

  }

  def first : Option[User] = {
    db withSession {
      val q = Users.map{ u => u}.take(1)
      q.list.headOption
    }
  }



  def all : List[User] = {
    db withSession {
      val q = Users.map({u => u}).sortBy(_.id)
      q.list
    }
  }


  def deleteAll = {
    db withSession {
      val q = for { 
        t <- Users 
      } yield t 

      q.mutate(_.delete) // deletes rows corresponding to query result 
    }
  }


}