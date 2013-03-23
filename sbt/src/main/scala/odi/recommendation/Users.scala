package odi.recommendation
import scala.slick.driver.PostgresDriver.simple._
import Database.threadLocalSession

// Definition of the USERS table
case class User(id: Option[Int] = None, name: String)
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
    }

    // return the found bid
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
  def delete(uid: Int) : Option[User] = {
    // get the bid we're deleting
    val result = get(uid);

    // delete the bid
    val toDelete = Ratings where (_.id === uid)
    db withSession {
      toDelete.delete
    }

    // return deleted bid
    result
  }

}