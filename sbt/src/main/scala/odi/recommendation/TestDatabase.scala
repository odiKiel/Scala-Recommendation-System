package odi.recommendation

object TestDatabase {
  /** create test databes entries */
  def setup = {
    Ratings.deleteAll
    SimilarItems.deleteAll
    SimilarUsers.deleteAll
    ItemTags.deleteAll
    Tags.deleteAll
    Items.deleteAll
    Users.deleteAll
    val item1 = Items.create(Item(None, "Verweildauer des Managements", 0.0, "http://localhost:9000/question/6/verweildauer-des-managements/", 6, 20000.0, 5000.0, 11))
    val item2 = Items.create(Item(None, "Management-Methoden", 0.0, "http://localhost:9000/question/8/management-methoden/", 8, 20000.0, 5000.0, 11))
    val item3 = Items.create(Item(None, "Obsolescence Management", 0.0, "http://localhost:9000/question/10/obsolescence-management/", 10, 20000.0, 5000.0, 11))
    val item4 = Items.create(Item(None, "Order Penetration Point", 0.0, "http://localhost:9000/question/12/order-penetration-point/", 12, 20000.0, 5000.0, 11))
    val item5 = Items.create(Item(None, "Phasenmodell", 0.0, "http://localhost:9000/question/14/phasenmodell/", 14, 20000.0, 5000.0, 11))

    val user1 = Users.create(User(None, "User1", 0.0, 1))
    val user2 = Users.create(User(None, "User2", 0.0, 2))
    val user3 = Users.create(User(None, "User3", 0.0, 3))
    val user4 = Users.create(User(None, "User4", 0.0, 4))
    val user5 = Users.create(User(None, "User5", 0.0, 5))

    val tag1 = Tags.create(Tag(None, "12085-6")) //Management
    val tag2 = Tags.create(Tag(None, "12766-3")) //Marketing

    ItemTags.create(ItemTag(None, item1.id.get, tag1.id.get))
    ItemTags.create(ItemTag(None, item2.id.get, tag1.id.get))
    ItemTags.create(ItemTag(None, item3.id.get, tag1.id.get))
    ItemTags.create(ItemTag(None, item4.id.get, tag1.id.get))
    ItemTags.create(ItemTag(None, item5.id.get, tag2.id.get))
    ItemTags.create(ItemTag(None, item5.id.get, tag1.id.get))

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

  /** create all tables */
  def initialSetup = {
    Users.createTable
    Items.createTable
    Tags.createTable
    ItemTags.createTable
    Ratings.createTable
    SimilarItems.createTable
    SimilarUsers.createTable

  }
}
