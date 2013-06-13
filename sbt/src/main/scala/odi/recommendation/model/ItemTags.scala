package odi.recommendation
import scala.slick.driver.PostgresDriver.simple._
import Database.threadLocalSession

import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}
import net.liftweb.json.JsonDSL._

/** Definition of the ItemTag table
 *
 * @param id the id of the entry
 * @param itemId the id of the item that is connected with the tag
 * @param tagId the id of the tag that is connected with the item
 */
case class ItemTag(id: Option[Int] = None, itemId: Int, tagId: Int) 

object ItemTags extends Table[ItemTag]("item_tags") with ModelTrait{
  def id = column[Int]("id", O.PrimaryKey, O.AutoInc) // This is the primary key column
  def itemId = column[Int]("item_id")
  def tagId = column[Int]("tag_id")
  def * = id.? ~ itemId ~ tagId <>(ItemTag, ItemTag.unapply _)
  def noID = itemId ~ tagId 

  /** creates the table of the itemTags **/
  def createTable = {
    db.withSession {
      ItemTags.ddl.create
    } 
  }

  /** returns all ItemTags **/
  def all : List[ItemTag] = {
    db withSession {
      val q = ItemTags.map({u => u}).sortBy(_.id)
      q.list
    }
  }

  /** creates a new itemTag 
    *
    * @param itemTag the itemTag that should be created
    * @return the itemTag with the id that was created
    */
  def create(itemTag: ItemTag): ItemTag = {
    var id: Int = -1;

    // start a db session
    db withSession {
    	val query = for (i <-ItemTags if i.itemId === itemTag.itemId && i.tagId === itemTag.tagId) yield i.id ~ i.itemId ~ i.tagId

    	val inter = query mapResult {
    	  case(id, itemId, tagId) => Option(ItemTag(Option(id), itemId, tagId))
    	}

      val result = inter.list match {
    	  case _ :: tail => inter.first
    	  case Nil => None
    	}
      if(result == None) {
        val res = ItemTags.noID insert (itemTag.itemId, itemTag.tagId)
        val idQuery = Query(SimpleFunction.nullary[Int]("LASTVAL"))
        id = idQuery.list().head
        new ItemTag(Option(id), itemTag.itemId, itemTag.tagId)
      }
      else {
        result.get
      }
    }
  }


  /** Delete an itemTag */
  def delete(id: Int) = {

    val toDelete = ItemTags where (_.id === id)
    db withSession {
      toDelete.delete
    }

  }

  /** deletes the itemTag by the tagId 
    *
    * @param tagId the tag id that should be removed from this table
    */
  def deleteByTagId(tagId: Int) = {

    val toDelete = ItemTags where (_.tagId === tagId)
    db withSession {
      toDelete.delete
    }

  }

  /** deletes an itemTag by the itemId
    *
    * @param itemId the item id that should be removed from this table
    */
  def deleteByItemId(itemId: Int) = {

    val toDelete = ItemTags where (_.itemId === itemId)
    db withSession {
      toDelete.delete
    }

  }


  /** deletes all itemTags */
  def deleteAll = {
    db withSession {
      val q = for { 
        t <- ItemTags 
      } yield t 

      q.mutate(_.delete) // deletes rows corresponding to query result 
    }
  }
}

