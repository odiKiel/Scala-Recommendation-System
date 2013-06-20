package odi.recommendation
import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}

/** this object creates a json string out of on object or decomposes a json string to an object */
object Json {

  /** decomposes a json string that comes from the 4store 
    * @param query the string that is returned by the 4store
    * @return a list of values from the string
    */
  def jsonToValue4Store(query: String): List[String] = {
    val json = parse(query)
    for {JField("value", JString(value)) <- json} yield value
  }

  /** decomposes a json string to a list
    * 
    * @param query the json string
    * @return the list of values
    */
  def jsonToList(query: String): List[String] = {
    val json = parse(query)
    for {JString(value) <- json} yield value
  }

  /** decomposes a json string to lists of lists
    *
    * @param query the json string
    * @return the list of lists
    */
  def jsonToLists(query: String): List[List[String]] = {
    implicit val formats = net.liftweb.json.DefaultFormats
    parse(query).extract[List[List[String]]]
  }

  /** creates a json string out of default scala types like lists
    * 
    * @param element the element that should be transformed into a string
    */
  def toJson[A <: AnyRef](element: A): String = {
    implicit val formats = net.liftweb.json.DefaultFormats
    write(element)
  }

}
