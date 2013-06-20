package odi.recommendation
import scala.slick.driver.PostgresDriver.simple._
import Database.threadLocalSession

import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}
import net.liftweb.json.JsonDSL._
import org.apache.commons.math3.linear._

 /** Definition of the SimilarItem table
   *
   * @param id the id of the entry
   * @param itemOneId the id of the first item
   * @param itemTwoId the id of the second item
   * @param similarity a double value that represents the similarity between those items
   */
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


  /** this object represents the SimilarItem table of the database **/
object SimilarItems extends Table[SimilarItem]("similar_items") with VectorCalculation with ModelTrait {
  def id = column[Int]("id", O.PrimaryKey, O.AutoInc) // This is the primary key column
  def itemOneId = column[Int]("item_one_id") 
  def itemTwoId = column[Int]("item_two_id")
  def similarity = column[Double]("similarity")
  def * = id.? ~ itemOneId ~ itemTwoId ~ similarity <>(SimilarItem, SimilarItem.unapply _)
  def noID = itemOneId ~ itemTwoId ~ similarity

  // A reified foreign key relation that can be navigated to create a join
  def itemOne = foreignKey("item_one_fk", itemOneId, Items)(_.id)
  def itemTwo = foreignKey("item_two_fk", itemTwoId, Items)(_.id)


                       
  /** create the SimilarItem table on the database **/
  def createTable = {
    db.withSession {
      SimilarItems.ddl.create
    } 

  }
 
  /** returns the SimilarItem for the id
    *
    * @param sid the id that represents the SimilarItem
    * @return None if it does not exist otherwise the SimilarItem
    */
  def get(sid: Int) : Option[SimilarItem] = {
    var result:Option[SimilarItem] = None;

    db withSession {
    	val query = for (s <-SimilarItems if s.id === sid) yield s.id ~ s.itemOneId ~ s.itemTwoId ~ s.similarity 

    	
    	val inter = query mapResult {
    	  case(id, itemOneId, itemTwoId, similarity) => Option(SimilarItem(Option(id), itemOneId, itemTwoId, similarity))
    	}

    	result = inter.list match {
    	  case _ :: tail => inter.first
    	  case Nil => None
    	}
    }

    // return the found bid
    result
  }

  /** returns the first SimilarItem of the database
    *
    * @return None if no SimilarItem exists otherwise the similarItem
    */
  def first : Option[SimilarItem] = {
    db withSession {
      val q = SimilarItems.map{ u => u}.take(1)
      q.list.headOption
    }
  }

 /** returns all similarItems for an item
   *
   * this method checks both similarItem entries wether one of them corresponds with the itemId
   * @param itemId the id of the item that is requested
   * @return a list of tuples with the itemId of the similar item and the similarity value
   */
  def byItemId(itemId: Int) : List[(Int, Double)] = {

    db withSession {
        // define the query and what we want as result
    	val queryItemOne = for (s <-SimilarItems if s.itemOneId === itemId ) yield s.itemTwoId ~ s.similarity 
      val queryItemTwo = for (s <-SimilarItems if s.itemTwoId === itemId ) yield s.itemOneId ~ s.similarity
    	
    	val listOne = queryItemOne mapResult {
    	  case(itemTwoId, similarity) => (itemTwoId, similarity)
    	}
      val listTwo = queryItemTwo mapResult {
        case(itemOneId, similarity) => (itemOneId, similarity)
      }


    	// check if there is one in the list and return it, or None otherwise
    	val listOneResult = listOne.list 
      val listTwoResult = listTwo.list
      listOneResult++listTwoResult
    }

  }


  /** this method returns the similarItem entry by two item ids
    *
    * @param item1Id the first item that should match
    * @param item2Id the second item that should match
    * @return the SimilarItem if it exists otherwise None
    */
  def getByItemItem(item1Id: Int, item2Id: Int) : Option[SimilarItem] = {
    var result:Option[SimilarItem] = None;

    db withSession {
    	val query = for (s <-SimilarItems if s.itemOneId === item1Id && s.itemTwoId === item2Id || s.itemOneId === item2Id && s.itemTwoId === item1Id) yield s.id ~ s.itemOneId ~ s.itemTwoId ~ s.similarity 

    	
    	val inter = query mapResult {
    	  case(id, itemOneId, itemTwoId, similarity) => Option(SimilarItem(Option(id), itemOneId, itemTwoId, similarity))
    	}

    	result = inter.list match {
    	  case _ :: tail => inter.first
    	  case Nil => None
    	}
    }

    result
  }

  /** return all SimilarItems 
    *
    * @return a list of SimilarItems
    */
  def all : List[SimilarItem] = {
    db withSession {
      val q = SimilarItems.map{u => u}.sortBy(_.itemTwoId).sortBy(_.itemOneId)
      q.list
    }
  }


  /** Create a similarItem if it doesnt exist or update if it has a new value
    *
    * @param similarItem the similarItem that should be created or updated 
    * @return the newly created or updated similarItem
    */
  def createOrUpdate(similarItem: SimilarItem): SimilarItem = {
    val oldSimilarItem: Option[SimilarItem] = getByItemItem(similarItem.itemOneId, similarItem.itemTwoId)
    if(oldSimilarItem == None ) {
      var id: Int = -1;

      db withSession {
        val res = SimilarItems.noID insert (similarItem.itemOneId.intValue, similarItem.itemTwoId.intValue, similarItem.similarity.floatValue)
        val idQuery = Query(SimpleFunction.nullary[Int]("LASTVAL"))
        id = idQuery.list().head
      }
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


  /** creates all similar items for one specific item
    *
    * this method takes an item id and a list of itemId similarity tuples
    * @param item1 the item id for that the similarities are created
    * @param item2Similarity a list of itemId and similarity values
    */
  def createAll(item1: Int, item2Similarity: collection.mutable.ArrayBuffer[(Int, Double)]) = {
    db withSession {
      item2Similarity.foreach{case(item2, similarity) => {
        SimilarItems.noID insert (item1, item2, similarity)
      }}
    }

  }

  /** Delete a SimilarItem
    *
    * @param sid the id of the SimilarItem that should be created
    * @return none if the similarItem does not exist or the SimilarItem otherwise
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


  /** deletes all similarItem entries that include the itemId
    *
    * @param iid the item id that should be removed from this table
    */
  def deleteByItemId(iid: Int) = {

    db withSession {
      val q = for (t <- SimilarItems if t.itemOneId === iid || t.itemTwoId === iid) yield t 

      q.mutate(_.delete) // deletes rows corresponding to query result 
    }


  }
  

  /** calculates the similarity between an item and all other items that are rated together with this item
    *
    * @param itemId the item that the similarities are created for
    * @param itemMapRatingsVector a triples with the other itemId and one rating vector for both items
    */
  def calculateSimilarity(itemId: Int, itemMapRatingsVector: collection.mutable.HashMap[Int, (RealVector, RealVector)]) = {
    println("calculate similarity for item: "+itemId)
    var i=1
    val itemSimilarity = collection.mutable.ArrayBuffer[(Int, Double)]()
    itemMapRatingsVector.foreach{case(currentItemId: Int, vectors: (RealVector, RealVector)) => 
      i+=1
      if(vectors._1.getDimension() > 2) {
            itemSimilarity += ((currentItemId, vectors._1.cosine(vectors._2)))
      }
    }
    createAll(itemId, itemSimilarity)
  }


  /** deletes all itemSimilarities */
  def deleteAll = {
    db withSession {
      val q = for { 
        t <- SimilarItems 
      } yield t 

      q.mutate(_.delete) // deletes rows corresponding to query result 
    }
  }

}
