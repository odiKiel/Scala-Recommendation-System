package odi.recommendation

object TestDatabase {
  def setup = {
    Ratings.deleteAll
    SimilarItems.deleteAll
    SimilarUsers.deleteAll
    Items.deleteAll
    Users.deleteAll
    val item1 = Items.create(Item(None, "Item1"))
    val item2 = Items.create(Item(None, "Item2"))
    val item3 = Items.create(Item(None, "Item3"))
    val item4 = Items.create(Item(None, "Item4"))
    val item5 = Items.create(Item(None, "Item5"))

    val user1 = Users.create(User(None, "User1", None))
    val user2 = Users.create(User(None, "User2", None))
    val user3 = Users.create(User(None, "User3", None))
    val user4 = Users.create(User(None, "User4", None))
    val user5 = Users.create(User(None, "User5", None))

    Ratings.create(Rating(None, item1.id.get, user1.id.get, 5, false))
    Ratings.create(Rating(None, item1.id.get, user2.id.get, 3, false))
    Ratings.create(Rating(None, item1.id.get, user3.id.get, 4, false))
    Ratings.create(Rating(None, item1.id.get, user4.id.get, 3, false))
    Ratings.create(Rating(None, item1.id.get, user5.id.get, 1, false))

    Ratings.create(Rating(None, item2.id.get, user1.id.get, 3, false))
    Ratings.create(Rating(None, item2.id.get, user2.id.get, 1, false))
    Ratings.create(Rating(None, item2.id.get, user3.id.get, 3, false))
    Ratings.create(Rating(None, item2.id.get, user4.id.get, 3, false))
    Ratings.create(Rating(None, item2.id.get, user5.id.get, 5, false))

    Ratings.create(Rating(None, item3.id.get, user1.id.get, 4, false))
    Ratings.create(Rating(None, item3.id.get, user2.id.get, 2, false))
    Ratings.create(Rating(None, item3.id.get, user3.id.get, 4, false))
    Ratings.create(Rating(None, item3.id.get, user4.id.get, 1, false))
    Ratings.create(Rating(None, item3.id.get, user5.id.get, 5, false))

    Ratings.create(Rating(None, item4.id.get, user1.id.get, 4, false))
    Ratings.create(Rating(None, item4.id.get, user2.id.get, 3, false))
    Ratings.create(Rating(None, item4.id.get, user3.id.get, 3, false))
    Ratings.create(Rating(None, item4.id.get, user4.id.get, 5, false))
    Ratings.create(Rating(None, item4.id.get, user5.id.get, 2, false))

    Ratings.create(Rating(None, item5.id.get, user2.id.get, 3, false))
    Ratings.create(Rating(None, item5.id.get, user3.id.get, 5, false))
    Ratings.create(Rating(None, item5.id.get, user4.id.get, 4, false))
    Ratings.create(Rating(None, item5.id.get, user5.id.get, 1, false))
  }

  def initialSetup = {
    Users.createTable
    Items.createTable
    Ratings.createTable
    SimilarItems.createTable
    SimilarUsers.createTable
  }
}
