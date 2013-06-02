package odi.recommendation

object TestDatabase {
  def setup = {
    Ratings.deleteAll
    SimilarItems.deleteAll
    SimilarUsers.deleteAll
    ItemTags.deleteAll
    Tags.deleteAll
    Items.deleteAll
    Users.deleteAll
    val item1 = Items.create(Item(None, "Item1", 0.0, "http://www.item1.de", 1, 5.0, 2.0, 11))
    val item2 = Items.create(Item(None, "Item2", 0.0, "http://www.item2.de", 2, 5.0, 2.0, 11))
    val item3 = Items.create(Item(None, "Item3", 0.0, "http://www.item3.de", 3, 5.0, 2.0, 11))
    val item4 = Items.create(Item(None, "Item4", 0.0, "http://www.item4.de", 4, 5.0, 2.0, 11))
    val item5 = Items.create(Item(None, "Item5", 0.0, "http://www.item5.de", 5, 5.0, 2.0, 11))

    val user1 = Users.create(User(None, "User1", 0.0, 1))
    val user2 = Users.create(User(None, "User2", 0.0, 2))
    val user3 = Users.create(User(None, "User3", 0.0, 3))
    val user4 = Users.create(User(None, "User4", 0.0, 4))
    val user5 = Users.create(User(None, "User5", 0.0, 5))

    val tag1 = Tags.create(Tag(None, "14888-0")) //Stoff
    val tag2 = Tags.create(Tag(None, "19024-5")) //Zwiebel

    ItemTags.create(ItemTag(None, item1.id.get, tag1.id.get))
    ItemTags.create(ItemTag(None, item2.id.get, tag1.id.get))
    ItemTags.create(ItemTag(None, item3.id.get, tag1.id.get))
    ItemTags.create(ItemTag(None, item4.id.get, tag1.id.get))
    ItemTags.create(ItemTag(None, item5.id.get, tag2.id.get))

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
