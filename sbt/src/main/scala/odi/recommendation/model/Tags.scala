package odi.recommendation
import scala.slick.driver.PostgresDriver.simple._
import Database.threadLocalSession

import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}
import net.liftweb.json.JsonDSL._

/** Definition of the Tag table */
case class Tag(id: Option[Int] = None, prefLabel: String) 

/** This object represents the tag table of the database */
object Tags extends Table[Tag]("tags") with ModelTrait{
  def id = column[Int]("id", O.PrimaryKey, O.AutoInc) // This is the primary key column
  def prefLabel = column[String]("pref_label")
  def * = id.? ~ prefLabel <>(Tag, Tag.unapply _)
  def noID = prefLabel 

  /* create a new table in the database */
  def createTable = {
    db.withSession {
      Tags.ddl.create
    } 
  }

  /** returns the tag by its descriptor 
   *
   * @param prefLabel the descriptor of the preferred label
   * @return the tag entry of the database
   */
  def byPrefLabel(prefLabel: String): Tag = {
    var result: Option[Tag] = None;

    db withSession {
    	val query = for (i <-Tags if i.prefLabel === prefLabel) yield i.id ~ i.prefLabel 

    	val inter = query mapResult {
    	  case(id, prefLabel) => Option(Tag(Option(id), prefLabel))
    	}

    	result = inter.list match {
    	  case _ :: tail => inter.first
    	  case Nil => None
    	}
    }
    if(result == None) {
      Tags.create(Tag(None, prefLabel))
    }
    else {
      result.get
    }


  }


  /* return all Tags from the database */
  def all : List[Tag] = {
    db withSession {
      val q = Tags.map({u => u}).sortBy(_.id)
      q.list
    }
  }

  /** creates a new tag 
    * 
    * @param the tag that should be created
    * @return the newly created tag
    */
  def create(tag: Tag): Tag = {
    var id: Int = -1;

    db withSession {
      val res = Tags.noID insert (tag.prefLabel)
      val idQuery = Query(SimpleFunction.nullary[Int]("LASTVAL"))
      id = idQuery.list().head
    }
    new Tag(Option(id), tag.prefLabel)
  }


  /** Delete a tag */
  def delete(id: Int) = {

    ItemTags.deleteByTagId(id)

    val toDelete = Tags where (_.id === id)
    db withSession {
      toDelete.delete
    }
  }

  /** delete all tags */
  def deleteAll = {
    db withSession {
      val q = for { 
        t <- Tags 
      } yield t 

      q.mutate(_.delete) // deletes rows corresponding to query result 
    }
  }
}

