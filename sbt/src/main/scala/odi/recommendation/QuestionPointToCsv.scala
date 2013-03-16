package odi.recommendation
import com.github.tototoshi.csv.CSVWriter
import java.io._
import xml._

object QuestionPointToCsv {
  var url = "questionpoint.org:80"
  val questionPointClient = new HttpClient(url)
  val queryStwClient = new HttpClient(Services("queryStwServer"))

  def apply(path: String) = {
    val ids = loadIdsFromQuestionPoint()
    println("done with loading keys")
    val qas = ids.map(id => {
      val qa = loadQuestionAnswer(id.text)
      writeToCsv(path, List(qa.toList))
      qa
    })
    questionPointClient.close()
    queryStwClient.close()
  }

  def loadIdsFromQuestionPoint(): Seq[Node] = {
    val keys = loadStw()
    //keys.flatMap(key => loadIdFromKey(key))
    loadIdFromKey("management")
  }

  def loadIdFromKey(key: String): Seq[Node] = {
    println("working with key: "+key)
    val response = questionPointClient.get(buildPathFromKey(key))
    val xml = XML.loadString(response.get())
    xml \ "records" \ "record" \ "id"
  }

    //returns id, question, answer
  def loadQuestionAnswer(id: String):List[String] = {
    println("Load new Answer")
    val qaXml = questionPointClient.get(buildPathFromId(id)) 
    val xml = XML.loadString(qaXml.get())
    val record = xml \ "record"
    val question = (record \ "question").text
    val answer = (record \ "answer").text
    List(id, question, answer)
  }

  def buildPathFromKey(key: String): String = {
    "/crs/servlet/org.oclc.kb.KBSearchWS?andk="+key+"&kbids=341&type=xml&preflang=6"
  }

  def buildPathFromId(id: String): String = {
    "http://questionpoint.org/crs/servlet/org.oclc.ask.AskPatronFetchQAWS?qid="+id+"&type=xml"
  }

  //should work as a Service
  def loadStw(): List[String] = {
    //queryStwClient.getAllStw()
    List("string")
  }

  def writeToCsv(path: String, data: List[List[String]]) = {
    val f = new File(path)
    val writer = CSVWriter.open(f, append = true)
    writer.writeAll(data)
    writer.close()
  }
}

