package odi.recommendation
import scala.slick.driver.PostgresDriver.simple._
import Database.threadLocalSession

import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}
import net.liftweb.json.JsonDSL._

 // Definition of the USER_ITEMS table
case class SimilarItem(id: Option[Int] = None, itemOneId: Int, itemTwoId: Int, similarity: Double) extends ToJson {
  def toJson = {
    val json = ("id"->id.get)~("itemOneId"->itemOneId)~("itemTwoId"->itemTwoId)~("similarity"->similarity)
    compact(render(json))
  }

  def similarityByItemId(itemId: Int): Option[(Int, Double)] = {
    if(itemId == itemOneId) {
      Some((itemTwoId, similarity))
    }
    else {
      if(itemId == itemTwoId) {
        Some((itemOneId, similarity))
      }
      else {
        None
      }
    }
  }
}
object SimilarItems extends Table[SimilarItem]("similar_items") with VectorCalculation {
  def id = column[Int]("id", O.PrimaryKey, O.AutoInc) // This is the primary key column
  def itemOneId = column[Int]("item_one_id") 
  def itemTwoId = column[Int]("item_two_id")
  def similarity = column[Double]("similarity")
  def * = id.? ~ itemOneId ~ itemTwoId ~ similarity <>(SimilarItem, SimilarItem.unapply _)
  def noID = itemOneId ~ itemTwoId ~ similarity

  // A reified foreign key relation that can be navigated to create a join
  def itemOne = foreignKey("item_one_fk", itemOneId, Items)(_.id)
  def itemTwo = foreignKey("item_two_fk", itemTwoId, Items)(_.id)

  lazy val db = Database.forURL("jdbc:postgresql://localhost/recommendation",
                         driver="org.postgresql.Driver",
                         user="oliver_diestel",
                         password="")
                       
  /*
                       .withSession {

    Ratings.ddl.create

    Ratings.insert(Rating(None, 1, 1, 4))
  }
    */

  def createTable = {
    db.withSession {
      SimilarItems.ddl.create
    } 

  }
 
  def get(sid: Int) : Option[SimilarItem] = {
    var result:Option[SimilarItem] = None;

    db withSession {
        // define the query and what we want as result
    	val query = for (s <-SimilarItems if s.id === sid) yield s.id ~ s.itemOneId ~ s.itemTwoId ~ s.similarity 

    	
    	val inter = query mapResult {
    	  case(id, itemOneId, itemTwoId, similarity) => Option(SimilarItem(Option(id), itemOneId, itemTwoId, similarity))
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

  def first : Option[SimilarItem] = {
    db withSession {
      val q = SimilarItems.map{ u => u}.take(1)
      q.list.headOption
    }
  }

  //returns a list of ItemId, Similarity
  def byItemId(itemId: Int) : List[(Int, Double)] = {

    db withSession {
        // define the query and what we want as result
    	val queryItemOne = for (s <-SimilarItems if s.itemOneId === itemId ) yield s.itemOneId ~ s.similarity 
      val queryItemTwo = for (s <-SimilarItems if s.itemTwoId === itemId ) yield s.itemTwoId ~ s.similarity
    	
    	val listOne = queryItemOne mapResult {
    	  case(itemOneId, similarity) => (itemOneId, similarity)
    	}
      val listTwo = queryItemTwo mapResult {
        case(itemTwoId, similarity) => (itemTwoId, similarity)
      }


    	// check if there is one in the list and return it, or None otherwise
    	val listOneResult = listOne.list 
      val listTwoResult = listTwo.list
      listOneResult++listTwoResult
    }

  }


  def getByItemItem(item1Id: Int, item2Id: Int) : Option[SimilarItem] = {
    var result:Option[SimilarItem] = None;

    db withSession {
        // define the query and what we want as result
    	val query = for (s <-SimilarItems if s.itemOneId === item1Id && s.itemTwoId === item2Id || s.itemOneId === item2Id && s.itemTwoId === item1Id) yield s.id ~ s.itemOneId ~ s.itemTwoId ~ s.similarity 

    	
    	val inter = query mapResult {
    	  case(id, itemOneId, itemTwoId, similarity) => Option(SimilarItem(Option(id), itemOneId, itemTwoId, similarity))
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

  def all : List[SimilarItem] = {
    db withSession {
      val q = SimilarItems.map{u => u}.sortBy(_.itemTwoId).sortBy(_.itemOneId)
      q.list
    }
  }


  /**
   * Create similarItem if it doesnt exist or update if it has a new value
   * todo check if threashold is good
   */
  def createOrUpdate(similarItem: SimilarItem): SimilarItem = {
    val oldSimilarItem: Option[SimilarItem] = getByItemItem(similarItem.itemOneId, similarItem.itemTwoId)
    if(oldSimilarItem == None ) {
      var id: Int = -1;

      // start a db session
      db withSession {
        // create a new bid
        val res = SimilarItems.noID insert (similarItem.itemOneId.intValue, similarItem.itemTwoId.intValue, similarItem.similarity.floatValue)
        // get the autogenerated bid
        val idQuery = Query(SimpleFunction.nullary[Int]("LASTVAL"))
        id = idQuery.list().head
      }
      // create a bid to return
      new SimilarItem(Option(id), similarItem.itemOneId, similarItem.itemTwoId, similarItem.similarity)
    }
    else {
      if(Math.abs(oldSimilarItem.get.similarity - similarItem.similarity) > 0.01){
        db withSession {
          val query = for (s <-SimilarItems if s.itemOneId === similarItem.itemOneId && s.itemTwoId === similarItem.itemTwoId) yield s.itemOneId ~ s.itemTwoId ~ s.similarity
          query.update((similarItem.itemOneId, similarItem.itemTwoId, similarItem.similarity))
        }
        similarItem
      }
      else {
        oldSimilarItem.get
      }
    }
  }

  /**
   * Delete a bid
   */
  def delete(sid: Int) : Option[SimilarItem] = {
    // get the bid we're deleting
    val result = get(sid);

    // delete the bid
    val toDelete = SimilarItems where (_.id === sid)
    db withSession {
      toDelete.delete
    }

    // return deleted bid
    result
  }

  def deleteByItemId(iid: Int) = {
    // get the bid we're deleting

    // delete the bid
    db withSession {
      val q = for (t <- SimilarItems if t.itemOneId === iid || t.itemTwoId === iid) yield t 

      q.mutate(_.delete) // deletes rows corresponding to query result 
    }


  }

  def calculateSimilarity(itemId: Int, itemIdUserIdHash: collection.mutable.HashMap[Int, List[Int]]) = {
    println("calculate similarity")
    var i=1
    itemIdUserIdHash.foreach{case(currentItemId: Int, userIdList: List[Int]) => 
      println("calculate similarity number: "+i)
      i+=1
      if(userIdList.length < 2) {
        //not enough ratings => no statement possible => save similarity of 0 (independence)
        SimilarItems.createOrUpdate(
          SimilarItem(
            None,
            itemId,
            currentItemId,
            0
          )
        )
      }
      else {
        SimilarItems.createOrUpdate(
          SimilarItem(
            None, 
            itemId, 
            currentItemId, 
            calculateItemSimilarityUsers(userIdList, itemId, currentItemId)
          )
        )
      }
    }
  }

  //calculate the similarity between two items with the users that rated both items
  def calculateItemSimilarityUsers(userIdList: List[Int], item1Id: Int, item2Id: Int): Double = {
    cosinusSimilarity(createRatingVector(item1Id, userIdList), createRatingVector(item2Id, userIdList))
  }

  def createRatingVector(itemId: Int, userIdList: List[Int]): Vector[Double] = {
    println("create rating vector")
    val result = userIdList match{
      case Nil => Vector[Double]()
      case head::Nil => Vector[Double](Ratings.byItemIdUserId(itemId, head).get.rating.toDouble)
      case head::tail => Ratings.byItemIdUserId(itemId, head).get.rating.toDouble +: createRatingVector(itemId, tail)
    }
    println("done creating rating vector")
    result
  }


  def deleteAll = {
    db withSession {
      val q = for { 
        t <- SimilarItems 
      } yield t 

      q.mutate(_.delete) // deletes rows corresponding to query result 
    }
  }

}
