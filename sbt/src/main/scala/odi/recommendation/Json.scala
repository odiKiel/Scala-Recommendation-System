package odi.recommendation
import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}

object Json {

  def jsonToValue4Store(query: String): List[String] = {
    val json = parse(query)
    for {JField("value", JString(value)) <- json} yield value
  }

  def jsonToList(query: String): List[String] = {
    val json = parse(query)
    for {JString(value) <- json} yield value
  }

  def listToJson(list: List[String]): String = {
    implicit val formats = Serialization.formats(NoTypeHints)
    write(list)
  }

}
