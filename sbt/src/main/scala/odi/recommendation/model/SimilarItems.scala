package odi.recommendation
import scala.slick.driver.PostgresDriver.simple._
import Database.threadLocalSession


 // Definition of the USER_ITEMS table
case class SimilarItem(id: Option[Int] = None, itemOneId: Int, itemTwoId: Int, similarity: Float) {
  def similarityByItemId(itemId: Int): Option[(Item, Float)] = {
    if(itemId == itemOneId) {
      Some((Items.get(itemTwoId).get, similarity))
    }
    else {
      if(itemId == itemTwoId) {
        Some((Items.get(itemOneId).get, similarity))
      }
      else {
        None
      }
    }
  }
}
object SimilarItems extends Table[SimilarItem]("similar_items") {
  def id = column[Int]("id", O.PrimaryKey, O.AutoInc) // This is the primary key column
  def itemOneId = column[Int]("item_one_id") 
  def itemTwoId = column[Int]("item_two_id")
  def similarity = column[Float]("similarity")
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

  def byItemId(itemId: Int) : List[SimilarItem] = {

    db withSession {
        // define the query and what we want as result
    	val query = for (s <-SimilarItems if s.itemOneId === itemId || s.itemTwoId === itemId) yield s.id ~ s.itemOneId ~ s.itemTwoId ~ s.similarity 

    	
    	val inter = query mapResult {
    	  case(id, itemOneId, itemTwoId, similarity) => SimilarItem(Option(id), itemOneId, itemTwoId, similarity)
    	}

    	// check if there is one in the list and return it, or None otherwise
    	inter.list 
    }

  }


  def getByItemItem(item1: Item, item2: Item) : Option[SimilarItem] = {
    var result:Option[SimilarItem] = None;

    db withSession {
        // define the query and what we want as result
    	val query = for (s <-SimilarItems if s.itemOneId === item1.id && s.itemTwoId === item2.id || s.itemOneId === item2.id && s.itemTwoId === item1.id) yield s.id ~ s.itemOneId ~ s.itemTwoId ~ s.similarity 

    	
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
      val q = SimilarItems.map{u => u}
      q.list
    }
  }


  /**
   * Create a bid using scala query. This will always create a new bid
   */
  def create(similarItem: SimilarItem): SimilarItem = {
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

  def calculateSimilarity(item: Item, similar: collection.mutable.HashMap[Item, List[User]]) = {
    similar.foreach((t: (Item, List[User])) => 
      SimilarItems.create(
        SimilarItem(
          None, 
          item.id.get, 
          t._1.id.get, 
          calculateItemSimilarityUsers(t._2, item, t._1)
        )
      )
    )
  }

  //calculate the similarity between two items with the users that rated both items
  def calculateItemSimilarityUsers(userList: List[User], item1: Item, item2: Item): Float = {
    cosinusSimilarity(createRatingVector(item1, userList), createRatingVector(item2, userList))
  }

  def createRatingVector(item: Item, userList: List[User]): Vector[Int] = {
    val itemId = item.id.get
    userList match{
      case Nil => Vector[Int]()
      case head::Nil => Vector[Int](Ratings.getByItemUser(itemId, head.id.get).get.rating)
      case head::tail => Ratings.getByItemUser(itemId, head.id.get).get.rating +: createRatingVector(item, tail)
    }
  }

  def cosinusSimilarity(itemVector1: Vector[Int], itemVector2: Vector[Int]): Float = {
    require(itemVector1.length == itemVector2.length, "Item Vector must be of same length")
    val numerator = vectorProduct(itemVector1, itemVector2)
    val denominator = vectorLength(itemVector1) * vectorLength(itemVector2)
    numerator / denominator
  }

  def vectorProduct(vector1: Vector[Int], vector2: Vector[Int]): Float = {
    vector1.zip(vector2).foldLeft(0)((result: Int, current: (Int, Int)) => result + current._1*current._2)
  }

  def vectorLength(vector: Vector[Int]): Float = {
    Math.sqrt(vector.map(Math.pow(_, 2)).sum).toFloat
  }
  def deleteAll = {
    all.foreach((u: SimilarItem) => delete(u.id.get))
  }


}
