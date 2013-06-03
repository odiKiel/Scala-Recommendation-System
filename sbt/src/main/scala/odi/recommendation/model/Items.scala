package odi.recommendation
import scala.slick.driver.PostgresDriver.simple._
import Database.threadLocalSession

import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}
import net.liftweb.json.JsonDSL._
// Definition of the ITEMS table
case class Item(id: Option[Int] = None, title: String, averageRating: Double, url: String, qId: Int, truncatedTimeSpend: Double, truncatedTimeScroll: Double, truncatedAmount: Int) extends ToJson with ModelTrait{
  def similarItems: List[(Int, Double)] = {
    SimilarItems.byItemId(this.id.get)
  }
  def toJson = {
    val json = ("id"->id.get)~("title"->title)~("averageRating"->averageRating)~("url"->url)~("qId"->qId)~("truncatedTimeSpend"->truncatedTimeSpend)~("truncatedTimeScroll"->truncatedTimeScroll)~("truncatedAmount"->truncatedAmount)
    compact(render(json))
  }
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

  def addTag(prefLabel: String) = {
    val tag = Tags.byPrefLabel(prefLabel)
    ItemTags.create(ItemTag(None, id.get, tag.id.get))
  }



}
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

  def createTable = {
    db.withSession {
      Items.ddl.create
    } 
  }

  def get(id: Int) : Option[Item] = {
    var result:Option[Item] = None;

    db withSession {
        // define the query and what we want as result
    	val query = for (i <-Items if i.id === id) yield i.id ~ i.title ~ i.averageRating ~ i.url ~ i.qId ~ i.truncatedTimeSpend ~ i.truncatedTimeScroll ~ i.truncatedAmount

    	// map the results to a Bid object
    	val inter = query mapResult {
    	  case(id, title, averageRating, url, qId, truncatedTimeSpend, truncatedTimeScroll, truncatedAmount) => Option(Item(Option(id), title, averageRating, url, qId, truncatedTimeSpend, truncatedTimeScroll, truncatedAmount))
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


  def byQId(qId: Int) : Option[Item] = {
    var result:Option[Item] = None;

    db withSession {
        // define the query and what we want as result
    	val query = for (i <-Items if i.qId === qId) yield i.id ~ i.title ~ i.averageRating ~ i.url ~ i.qId ~ i.truncatedTimeSpend ~ i.truncatedTimeScroll ~ i.truncatedAmount

    	// map the results to a Bid object
    	val inter = query mapResult {
    	  case(id, title, averageRating, url, qId, truncatedTimeSpend, truncatedTimeScroll, truncatedAmount) => Option(Item(Option(id), title, averageRating, url, qId, truncatedTimeSpend, truncatedTimeScroll, truncatedAmount))
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

  def allIds : List[Int] = {
    db withSession {
        // define the query and what we want as result
    	val query = for (r <-Items) yield r.id

    	// map the results to a Bid object
      val inter = query mapResult {
    	  case(id) => id
    	}

    	// check if there is one in the list and return it, or None otherwise
      inter.list
    }
  }

  //get all items from a specific user
  def allItemIdsUserId(userId: Int) : List[Int] = {
    db withSession {
        // define the query and what we want as result
    	val query = for (r <-Ratings if r.userId === userId;
                       i <- Items if i.id === r.itemId) yield i.id 

    	// map the results to a Bid object
    	val inter = query mapResult {
    	  case(id) => id
    	}

    	// check if there is one in the list and return it, or None otherwise
      inter.list
    }
  }

  def allItemIdsForUserIdWithRating(userId: Int) : List[(Int, Int)] = {
    db withSession {
        // define the query and what we want as result
    	val query = for (r <-Ratings if r.userId === userId;
                       i <- Items if i.id === r.itemId) yield i.id ~ r.rating 

    	// map the results to a Bid object
    	val inter = query mapResult {
    	  case(id, rating) => (id, rating)
    	}

    	// check if there is one in the list and return it, or None otherwise
      inter.list
    }
  }

  def allItemIdsForUserIdWithRatingNormalized(userId: Int) : List[(Int, Double)] = {
    db withSession {
        // define the query and what we want as result
    	val query = for (r <-Ratings if r.userId === userId;
                       u <- Users if u.id === userId;
                       i <- Items if i.id === r.itemId) yield i.id ~ r.rating ~ u.averageRating

    	// map the results to a Bid object
    	val inter = query mapResult {
    	  case(id, rating, averageRating) => (id, rating-averageRating)
    	}

    	// check if there is one in the list and return it, or None otherwise
      inter.list
    }
  }


  def calculateAverageRating = {
    val time = System.nanoTime
    Items.all.foreach((item: Item) => item.calculateAverageRating)
    println("time: "+(System.nanoTime-time))
  }

  def first : Option[Item] = {
    
    db withSession {
      val q = Items.map{ u => u}
      q.sortBy(_.id).take(1).list.headOption
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

      val res = Items.noID insert (item.title, item.averageRating, item.url, item.qId, item.truncatedTimeSpend, item.truncatedTimeScroll, item.truncatedAmount)
      // get the autogenerated bid
      val idQuery = Query(SimpleFunction.nullary[Int]("LASTVAL"))
      id = idQuery.list().head
    }
    // create a bid to return
    new Item(Option(id), item.title, item.averageRating, item.url, item.qId, item.truncatedTimeSpend, item.truncatedTimeScroll, item.truncatedAmount)
  }


  /**
   * Delete a bid
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

  def deleteAll = {
    db withSession {
      val q = for { 
        t <- Items 
      } yield t 

      q.mutate(_.delete) // deletes rows corresponding to query result 
    }
  }
}

