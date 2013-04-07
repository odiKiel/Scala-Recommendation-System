package odi.recommendation
import org.jboss.netty.handler.codec.http.{HttpResponse}
import com.twitter.finagle.builder.Server
import com.twitter.util.{Promise, Future}
import java.io._

//might get trouble with 'umlaute'
object WebService extends HttpServer {

  val name = "WebService"

  def apply(port: Int): Int = {
    super.apply(port, name)
  }

  def callPostMethod(path: Array[String], value: String): Future[HttpResponse] = {
    path.head match {
      case _ => Future.value(createHttpResponse("No such method"))
    }
  }

  def callGetMethod(path: Array[String]): Future[HttpResponse] = {
    path.head match {
      case "data" => getData(path)
      case "file" => getFile(path.tail)
      case _ => Future.value(createHttpResponse("No such method"))
    }
  }

  def getFile(file: Array[String]): Future[HttpResponse] = {
    val path = file.mkString("/")
    if(path.matches("""[\/\w-]+\.\w+""")) {
      val source = scala.io.Source.fromFile("public/"+path)
      val lines = source.mkString
      source.close()
      Future.value(createHttpResponse(lines))
    }
    else {
      Future.value(createHttpResponse("not a valid path"))
    }
  }

  def getData(path: Array[String]): Future[HttpResponse] = {
    Future.value(createHttpResponse("data"))
  }

  def getTagWord(value: String): Future[HttpResponse] = {
    //run levenshtein distance algorithm with the word and all stw thesaurus words
    Future.value(createHttpResponse("string"))
  }

}

