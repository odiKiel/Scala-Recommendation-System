package odi.recommendation
import scala.slick.driver.PostgresDriver.simple._
import Database.threadLocalSession

import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}
import net.liftweb.json.JsonDSL._
// Definition of the ITEMS table
case class Item(id: Option[Int] = None, title: String) extends ToJson{
  def similarItems: List[(Item, Double)] = {
    SimilarItems.byItemId(this.id.get).map((si: SimilarItem) => si.similarityByItemId(this.id.get).get)
  }
  def toJson = {
    val json = ("id"->id.get)~("title"->title)
    compact(render(json))
  }

}
object Items extends Table[Item]("items") {
  def id = column[Int]("id", O.PrimaryKey, O.AutoInc) // This is the primary key column
  def title = column[String]("title")
  def * = id.? ~ title <>(Item, Item.unapply _)
  def noID = title 

  lazy val db = Database.forURL("jdbc:postgresql://localhost/recommendation",
                         driver="org.postgresql.Driver",
                         user="oliver_diestel",
                         password="")
  def createTable = {
    db.withSession {
      Items.ddl.create
    } 
  }

  def get(id: Int) : Option[Item] = {
    var result:Option[Item] = None;

    db withSession {
        // define the query and what we want as result
    	val query = for (i <-Items if i.id === id) yield i.id ~ i.title 

    	// map the results to a Bid object
    	val inter = query mapResult {
    	  case(id, title) => Option(Item(Option(id), title))
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

  def all : List[Item] = {
    db withSession {
      val q = Items.map({u => u}).sortBy(_.id)
      q.list
    }
  }

  //get all items from a specific user
  def allItemsUser(userId: Int) : List[Item] = {
    var result:List[Item] = List[Item]()

    db withSession {
        // define the query and what we want as result
    	val query = for (r <-Ratings if r.userId === userId;
                       i <- Items if i.id === r.itemId) yield i.id ~ i.title

    	// map the results to a Bid object
    	val inter = query mapResult {
    	  case(id, title) => Option(Item(Option(id), title))
    	}

    	// check if there is one in the list and return it, or None otherwise
      if(inter.list.length > 0) {
        result = inter.list.flatten
      }
    }
    result
  }

  def first : Option[Item] = {
    db withSession {
      val q = Items.map{ u => u}.take(1)
      q.list.headOption
    }
  }

  /**
   * Create a bid using scala query. This will always create a new bid
   */
  def create(item: Item): Item = {
    var id: Int = -1;

    // start a db session
    db withSession {
      // create a new bid
      val res = Items.noID insert (item.title)
      // get the autogenerated bid
      val idQuery = Query(SimpleFunction.nullary[Int]("LASTVAL"))
      id = idQuery.list().head
    }
    // create a bid to return
    new Item(Option(id), item.title)
  }


  /**
   * Delete a bid
   */
  def delete(id: Int) : Option[Item] = {
    // get the bid we're deleting
    val result = get(id);

    // delete the bid
    val toDelete = Items where (_.id === id)
    db withSession {
      toDelete.delete
    }

    // return deleted bid
    result
  }

  def deleteAll = {
    db withSession {
      val q = for { 
        t <- Items 
      } yield t 

      q.mutate(_.delete) // deletes rows corresponding to query result 
    }
  }
}

