package odi.recommendation
import scala.slick.driver.PostgresDriver.simple._
import Database.threadLocalSession

/** this model trait is responsible for managing the database */
trait ModelTrait {
  
  /*
  lazy val db = Database.forURL("jdbc:postgresql://localhost/recommendation",
                         driver="org.postgresql.Driver",
                         user="oliver_diestel",
                         password="")
                       */

  lazy val db = Database.forURL("jdbc:postgresql://localhost/recommendation_test",
                         driver="org.postgresql.Driver",
                         user="oliver_diestel",
                         password="")

 
}
