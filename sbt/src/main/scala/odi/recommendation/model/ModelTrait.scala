package odi.recommendation
import scala.slick.driver.PostgresDriver.simple._
import Database.threadLocalSession

//this should be a trait that has all the standart model functions
trait ModelTrait {
  lazy val db = Database.forURL("jdbc:postgresql://localhost/recommendation",
                         driver="org.postgresql.Driver",
                         user="oliver_diestel",
                         password="")
 
}
