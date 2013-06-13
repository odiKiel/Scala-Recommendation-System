package odi.recommendation
import scala.slick.driver.PostgresDriver.simple._
import Database.threadLocalSession

import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}
import net.liftweb.json.JsonDSL._

/** Definition of the ITEM table
  *
  * This class represents an item in the database
  * @param id this is the optional id for the item if a new id is created None is sufficient
  * @param title this is the title of the item
  * @param averageRating this is the average rating of the item
  * @param url this is the url of the item
  * @param qId this is the item id in the question and answer system
  * @param truncatedTimeSpend this is the truncated time that all system users spend on the item page
  * @param truncatedTimeScroll this is the truncated time that all system users scrolled on the item page
  * @param truncatedAmount this is the amount of all users that added a time to this item
  */
case class Item(id: Option[Int] = None, title: String, averageRating: Double, url: String, qId: Int, truncatedTimeSpend: Double, truncatedTimeScroll: Double, truncatedAmount: Int) extends ToJson with ModelTrait{
  def similarItems: List[(Int, Double)] = {
    SimilarItems.byItemId(this.id.get)
  }

  /** returns this item as a Json string */
  def toJson = {
    val json = ("id"->id.get)~("title"->title)~("averageRating"->averageRating)~("url"->url)~("qId"->qId)~("truncatedTimeSpend"->truncatedTimeSpend)~("truncatedTimeScroll"->truncatedTimeScroll)~("truncatedAmount"->truncatedAmount)
    compact(render(json))
  }

  /** calculates the average rating for this item */
  def calculateAverageRating = {
    println("calculate average rating for Item"+id.get)
    val ratings = Ratings.byItemId(id.get)
    val averageRating = if(ratings.length == 0) {
      3.0 //if no ratings for this item use the scale middle
    }
    else {
      ratings.map(_.rating).sum / ratings.length.toDouble
    }
    db withSession {
      val query = for (i <-Items if i.id === id.get ) yield i.averageRating 
      query.update(averageRating)
    }
  }

  /** adds a tag to this item */
  def addTag(prefLabel: String) = {
    println("addTag: "+prefLabel)
    val tag = Tags.byPrefLabel(prefLabel)
    ItemTags.create(ItemTag(None, id.get, tag.id.get))
  }


}


/** this object represents the item table in the database */
object Items extends Table[Item]("items") with ModelTrait{
  def id = column[Int]("id", O.PrimaryKey, O.AutoInc) // This is the primary key column
  def title = column[String]("title")
  def averageRating = column[Double]("average_rating")
  def url = column[String]("url")
  def qId = column[Int]("q_id")
  def truncatedTimeSpend = column[Double]("truncated_time_spend")
  def truncatedTimeScroll = column[Double]("truncated_time_scroll")
  def truncatedAmount = column[Int]("truncated_amount")
  def * = id.? ~ title ~ averageRating ~ url ~ qId ~ truncatedTimeSpend ~ truncatedTimeScroll ~ truncatedAmount <>(Item, Item.unapply _)
  def noID = title ~ averageRating ~ url ~ qId ~ truncatedTimeSpend ~ truncatedTimeScroll ~ truncatedAmount

  /** creates the table for this item in the database */
  def createTable = {
    db.withSession {
      Items.ddl.create
    } 
  }

  /** returns an Item for an id */
  def get(id: Int) : Option[Item] = {
    var result:Option[Item] = None;

    db withSession {
    	val query = for (i <-Items if i.id === id) yield i.id ~ i.title ~ i.averageRating ~ i.url ~ i.qId ~ i.truncatedTimeSpend ~ i.truncatedTimeScroll ~ i.truncatedAmount

    	val inter = query mapResult {
    	  case(id, title, averageRating, url, qId, truncatedTimeSpend, truncatedTimeScroll, truncatedAmount) => Option(Item(Option(id), title, averageRating, url, qId, truncatedTimeSpend, truncatedTimeScroll, truncatedAmount))
    	}

    	// check if there is one in the list and return it, or None otherwise
    	result = inter.list match {
    	  case _ :: tail => inter.first
    	  case Nil => None
    	}
    }

    result
  }


  /** returns an item by the id of the question and answer system */
  def byQId(qId: Int) : Option[Item] = {
    var result:Option[Item] = None;

    db withSession {
    	val query = for (i <-Items if i.qId === qId) yield i.id ~ i.title ~ i.averageRating ~ i.url ~ i.qId ~ i.truncatedTimeSpend ~ i.truncatedTimeScroll ~ i.truncatedAmount

    	val inter = query mapResult {
    	  case(id, title, averageRating, url, qId, truncatedTimeSpend, truncatedTimeScroll, truncatedAmount) => Option(Item(Option(id), title, averageRating, url, qId, truncatedTimeSpend, truncatedTimeScroll, truncatedAmount))
    	}

    	// check if there is one in the list and return it, or None otherwise
    	result = inter.list match {
    	  case _ :: tail => inter.first
    	  case Nil => None
    	}
    }

    result
  }


  /** returns all items as a list */
  def all : List[Item] = {
    db withSession {
      val q = Items.map({u => u}).sortBy(_.id)
      q.list
    }
  }

  /** returns all ids of all items */
  def allIds : List[Int] = {
    db withSession {
    	val query = for (r <-Items) yield r.id

      val inter = query mapResult {
    	  case(id) => id
    	}

      inter.list
    }
  }

  /** returns all item ids that are rated by a specific user */
  def allItemIdsUserId(userId: Int) : List[Int] = {
    db withSession {
    	val query = for (r <-Ratings if r.userId === userId;
                       i <- Items if i.id === r.itemId) yield i.id 

    	val inter = query mapResult {
    	  case(id) => id
    	}
      inter.list
    }
  }

  /** returns all item ids with the rating for a specific user */
  def allItemIdsForUserIdWithRating(userId: Int) : List[(Int, Int)] = {
    db withSession {
    	val query = for (r <-Ratings if r.userId === userId;
                       i <- Items if i.id === r.itemId) yield i.id ~ r.rating 

    	val inter = query mapResult {
    	  case(id, rating) => (id, rating)
    	}

      inter.list
    }
  }

  /** returns all item ids with the rating for a specific user
    * the rating is subtracted by the average rating of that user
    */
  def allItemIdsForUserIdWithRatingNormalized(userId: Int) : List[(Int, Double)] = {
    db withSession {
    	val query = for (r <-Ratings if r.userId === userId;
                       u <- Users if u.id === userId;
                       i <- Items if i.id === r.itemId) yield i.id ~ r.rating ~ u.averageRating

    	val inter = query mapResult {
    	  case(id, rating, averageRating) => (id, rating-averageRating)
    	}

      inter.list
    }
  }


  /** calculates the average ratings for all items */
  def calculateAverageRating = {
    val time = System.nanoTime
    Items.all.foreach((item: Item) => item.calculateAverageRating)
    println("time: "+(System.nanoTime-time))
  }

  /** returns the item with the lowest id */
  def first : Option[Item] = {
    
    db withSession {
      val q = Items.map{ u => u}
      q.sortBy(_.id).take(1).list.headOption
    }
  }

  /** Create an item using scala query. */
  def create(item: Item): Item = {
    var id: Int = -1;

    db withSession {

      val res = Items.noID insert (item.title, item.averageRating, item.url, item.qId, item.truncatedTimeSpend, item.truncatedTimeScroll, item.truncatedAmount)
      val idQuery = Query(SimpleFunction.nullary[Int]("LASTVAL"))
      id = idQuery.list().head
    }
    new Item(Option(id), item.title, item.averageRating, item.url, item.qId, item.truncatedTimeSpend, item.truncatedTimeScroll, item.truncatedAmount)
  }


  /** Delete an item 
    *
    * this method also deletes the rating and the similar item for the specified item
    */
  def delete(id: Int) : Option[Item] = {
    val result = get(id);

    //delete ratings and similaritems with this id
    Ratings.deleteByItemId(id)
    SimilarItems.deleteByItemId(id)

    val toDelete = Items where (_.id === id)
    db withSession {
      toDelete.delete
    }

    result
  }

  /** deletes all items */
  def deleteAll = {
    db withSession {
      val q = for { 
        t <- Items 
      } yield t 

      q.mutate(_.delete) // deletes rows corresponding to query result 
    }
  }
}

