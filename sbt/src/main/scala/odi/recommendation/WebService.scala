package odi.recommendation
import org.jboss.netty.handler.codec.http.{HttpResponse}
import com.twitter.finagle.builder.Server
import com.twitter.util.{Promise, Future}
import java.io._
import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}

//might get trouble with 'umlaute'
object WebService extends HttpServer {

  val name = "WebService"
  //for json parsing
  implicit val formats = DefaultFormats 

  def apply(port: Int): Int = {
    super.apply(port, name)
  }

  def callPostMethod(path: Array[String], value: String): Future[HttpResponse] = {
    path.head match {
      case "data" => saveData(path.tail, value)
      case _ => Future.value(createHttpResponse("No such method"))
    }
  }

  def saveData(path: Array[String], value: String): Future[HttpResponse] = {
    println("saving json "+value)
    val data = parse(value)
    println("saving data"+data)
    path.head match {
      case "ratings" => {
        val rating = data.extract[Rating]
        val response = Ratings.create(rating)
        Future.value(createHttpResponse(response.toJson))
      }
      case "users" => {
        val user = data.extract[User]
        val response = Users.create(user)
        Future.value(createHttpResponse(response.toJson))
      }
      case "items" => {
        val item = data.extract[Item]
        val response = Items.create(item)
        Future.value(createHttpResponse(response.toJson))
      }
      case _ => Future.value(createErrorHttpResponse)
    }
  }


  def callGetMethod(path: Array[String]): Future[HttpResponse] = {
    path.head match {
      case "" => getFile(Array("index.html"))
      case "data" => getData(path.tail)
      case "file" => getFile(path.tail)
      case _ => Future.value(createHttpResponse("No such method"))
    }
  }

  //file not found exception
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

  //should work with all models
  def getData(path: Array[String]): Future[HttpResponse] = {
    path.length match {
      case 1 => getAll(path(0))
      case 2 => getOne(path(0), path(1).toInt)
      case _ => Future.value(createErrorHttpResponse)
    }
  }

  def getAll(kind: String): Future[HttpResponse] = {
    val response = kind match {
      case "ratings" => Ratings.all.map(_.toJson)
      case "items" => Items.all.map(_.toJson)
      case "users" => Users.all.map(_.toJson)
    }
    Future.value(createHttpResponse("["+response.mkString(", ")+"]"))
  }

  def getOne(kind: String, id: Int): Future[HttpResponse] = {
    println("type: "+kind+" id: "+id)
    kind match {
      case "ratings" => createHttpResponseForOption(Ratings.get(id))
      case "items" => createHttpResponseForOption(Items.get(id))
      case "users" => createHttpResponseForOption(Users.get(id))
    }
  }

  def createHttpResponseForOption(e: Option[ToJson]): Future[HttpResponse] = {
    if(e == None) {
      Future.value(createErrorHttpResponse)
    }
    else {
      Future.value(createHttpResponse(e.get.toJson))
    }
  }


}

