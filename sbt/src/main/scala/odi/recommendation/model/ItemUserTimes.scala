package odi.recommendation
import scala.slick.driver.PostgresDriver.simple._
import Database.threadLocalSession

import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}
import net.liftweb.json.JsonDSL._

/** Definition of the ItemUserTime table
  *
  * @param id the optional id of the item user time table None if it is a new entry
  * @param itemId the item that the time is added to
  * @param userId the user that spend the time on the item page
  * @param timeSpend the time the user spend on the website
  * @param timeScroll the time the user scrolled on the website
  */
case class ItemUserTime(id: Option[Int] = None, itemId: Int, userId: Int, timeSpend: Double, timeScroll: Double) 

/** this object represents the ItemUserTime table of the database */
object ItemUserTimes extends Table[ItemUserTime]("item_user_times") with ModelTrait{
  def id = column[Int]("id", O.PrimaryKey, O.AutoInc) // This is the primary key column
  def itemId = column[Int]("item_id")
  def userId = column[Int]("user_id")
  def timeSpend = column[Double]("time_spend")
  def timeScroll = column[Double]("time_scroll")
  def * = id.? ~ itemId ~ userId ~ timeSpend ~ timeScroll<>(ItemUserTime, ItemUserTime.unapply _)
  def noID = itemId ~ userId ~ timeSpend ~ timeScroll 

  /** creates the ItemUserTime table in the database */
  def createTable = {
    db.withSession {
      ItemUserTimes.ddl.create
    } 
  }

  /** returns all ItemUserTimes */
  def all : List[ItemUserTime] = {
    db withSession {
      val q = ItemUserTimes.map({u => u}).sortBy(_.id)
      q.list
    }
  }


  /** creates on Item UserTime entry
    *
    * creates a new itemUserTime object if no object exist for the user item combination
    * otherwise adds the time to the existing object
    * @param itemUserTime the itemUserTime object that should be created
    * @return the created itemUserTime object
    */
  def create(itemUserTime: ItemUserTime): ItemUserTime = {
    var id: Int = -1;

    // start a db session
    db withSession {
      //try to load an existing object with the item and user id
    	val query = for (i <-ItemUserTimes if i.itemId === itemUserTime.itemId && i.userId === itemUserTime.userId) yield i.id ~ i.itemId ~ i.userId ~ i.timeSpend ~ i.timeScroll

    	val inter = query mapResult {
    	  case(id, itemId, userId, timeSpend, timeScroll) => Option(ItemUserTime(Option(id), itemId, userId, timeSpend, timeScroll))
    	}

      val result = inter.list match {
    	  case _ :: tail => inter.first
    	  case Nil => None
    	}

      //if the object exists add the times otherwise create a new object
      if(result == None) {
        // create a new bid
        val res = ItemUserTimes.noID insert (itemUserTime.itemId, itemUserTime.userId, itemUserTime.timeSpend, itemUserTime.timeScroll)
        // get the autogenerated bid
        val idQuery = Query(SimpleFunction.nullary[Int]("LASTVAL"))
        id = idQuery.list().head
        new ItemUserTime(Option(id), itemUserTime.itemId, itemUserTime.userId, itemUserTime.timeSpend, itemUserTime.timeScroll)
      }
      else {
        val oldItemUserTime = result.get
        db withSession {
          val query = for (i <-ItemUserTimes if i.id === oldItemUserTime.id.get) yield i.id ~ i.itemId ~ i.userId ~ i.timeSpend ~ i.timeScroll
          query.update((oldItemUserTime.id.get, itemUserTime.itemId, itemUserTime.userId, (itemUserTime.timeSpend+oldItemUserTime.timeSpend), (itemUserTime.timeScroll+oldItemUserTime.timeScroll)))
        }
        new ItemUserTime(oldItemUserTime.id, itemUserTime.itemId, itemUserTime.userId, (itemUserTime.timeSpend+oldItemUserTime.timeSpend), (itemUserTime.timeScroll+oldItemUserTime.timeScroll))
      }
    }
    // create a bid to return
  }


  /** Delete an ItemUserTime object */
  def delete(id: Int) = {

    val toDelete = ItemUserTimes where (_.id === id)
    db withSession {
      toDelete.delete
    }

  }


  /** Delete all ItemUserTimes */
  def deleteAll = {
    db withSession {
      val q = for { 
        t <- ItemUserTimes 
      } yield t 

      q.mutate(_.delete) // deletes rows corresponding to query result 
    }
  }
}

