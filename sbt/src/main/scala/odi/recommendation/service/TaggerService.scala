package odi.recommendation
import org.jboss.netty.handler.codec.http.{HttpResponse}
import com.twitter.finagle.builder.Server
import com.twitter.util.{Promise, Future}


//might get trouble with 'umlaute'
object TaggerService extends HttpServer {
  QueryStwServer(Services("queryStwServer").toInt)
  LevenshteinDistanceService(Services("levenshteinDistanceService").toInt)

  val queryStwClient = new HttpClient("localhost:"+Services("queryStwServer"))
  val levenshteinDistanceClient = new HttpClient("localhost:"+Services("levenshteinDistanceService"))
  //val taggerPool = FuturePool(Executors.newFixedThreadPool(4))

  val name = "TaggerService"

  def apply(port: Int): Int = {
    super.apply(port, name)
  }

  def callPostMethod(path: Array[String], value: String): Future[HttpResponse] = {
    path.head match {
      case "tagText" => postTagText(path.tail, value)  //returns the tags for a text
      case "prefLabelText" => postPrefLabelText(path.tail, value) //returns the preferred labels for a text
      case "tagsPrefLabels" => postTagsPrefLabels(path.tail, value)  //returns the tags for the preferred labels
      case "prefLabelsTags" => postPrefLabelsTags(path.tail, value) //ruturns the preferred labels for the tags
      case _ => Future.value(createHttpResponse("No such method"))
    }
  }

  def callGetMethod(path: Array[String]): Future[HttpResponse] = {
    path.head match {
      case _ => Future.value(createHttpResponse("No such method"))
    }
  }

  def postTagText(args: Array[String], value: String): Future[HttpResponse] = {
    println(value)
    val r = new Promise[HttpResponse]
    tagText(value) onSuccess { tags => 
      println(tags)
      r.setValue(createHttpResponse(Json.toJson(tags.toList)))
    }
    r
  }

  def postPrefLabelText(args: Array[String], value: String): Future[HttpResponse] = {
    val r = new Promise[HttpResponse]
    tagText(value) onSuccess { tags => 
      val prefLabels = generatePrefLabelForTags(tags.toList)
      println("done with pref labels: "+prefLabels)
  
      //r.setValue(createHttpResponse(Json.toJson(Future.collect(prefLabelFuture).get())))
      r.setValue(createHttpResponse(Json.toJson(prefLabels))) // speed hack only words if TaggerService and QuerySTw run on the same server
    }

    r
  }

  def postPrefLabelsTags(args: Array[String], value: String): Future[HttpResponse] = {
    val tags = Json.jsonToList(value)
    val prefLabels = generatePrefLabelForTags(tags)
    Future.value(createHttpResponse(Json.toJson(prefLabels)))
  }

  def postTagsPrefLabels(args: Array[String], value: String): Future[HttpResponse] = {

    val tags = Json.jsonToList(value)
    val prefLabels = generateTagForPrefLabels(tags)
  
      //r.setValue(createHttpResponse(Json.toJson(Future.collect(prefLabelFuture).get())))
      Future.value(createHttpResponse(Json.toJson(prefLabels))) // speed hack only words if TaggerService and QuerySTw run on the same server

  }

  def generateTagForPrefLabels(prefLabels: List[String]): List[String] = {
    prefLabels.flatMap(prefLabel => {
        QueryStwServer.findTagByPrefLabel(prefLabel)
      }).distinct
  }

  def generatePrefLabelForTags(tags: List[String]): List[String] = {
    println("generatePrefLabels!!!")
    tags.flatMap(tag => {
        //queryStwClient.get("/prefLabels/"+tag)
        val time = System.nanoTime
        println("Time before query: "+(System.nanoTime - time))
        val result = QueryStwServer.findPrefLabel(tag) // speed hack only works if TaggerService and QuerySTw run on the same server
        println("Time after query: "+(System.nanoTime - time))
        result
      }).distinct
  }


  //only use runQueryWithPagination query with ORDER BY

  //Im working with Json strings!
  val query = "PREFIX skos: <http://www.w3.org/2004/02/skos/core#> PREFIX xml: <http://zbw.eu/stw/> SELECT ?q WHERE { ?s ?p ?q FILTER(?p = skos:prefLabel || ?p = skos:altLabel)} ORDER BY ?q"
  val pagination = 1000

  def tagText(text: String): Future[Seq[String]] = {
    val time = System.nanoTime
    val textList = text.split("\\W").toList
    if(!(query contains "ORDER BY")) {
     throw new IllegalArgumentException("query needs to use ORDER BY")
    }


    val dfsmList = createDfsm(textList)
    val dfsmTime = System.nanoTime-time

    tagTextRec(dfsmList, pagination, 0)

  }

  def tagTextRec(dfsmList: List[DeterministicFiniteStateMachine], pagination: Int, offset: Int): Future[Seq[String]] = {
    if(offset > 32000) {
      return Future.value(Seq())
    }

    val test = queryStwClient.post("/runQuery/", query + " LIMIT " + pagination + " OFFSET "+offset) flatMap {labelsJson =>
      val labels = Json.jsonToValue4Store(labelsJson)
      println(labels.length)
      val seqString = labels map {label => {
          var ret: Option[String] = None
          if(labelForText(label, dfsmList)) {
            ret = Some(label)
          }
          ret
        } 
      }
      val futureSeqString1 = Future.value(seqString.flatten) 
      val futureSeqString = tagTextRec(dfsmList, pagination, offset+1000)
      val seqFuture = Seq(futureSeqString1, futureSeqString)
      Future.collect(seqFuture) map {_.flatten}
    }
    test
  }

  def createDfsm(text: List[String]): List[DeterministicFiniteStateMachine] = {
    text.map((word: String) => {
      val fsm = new FiniteStateMachine()
      val degree = if(word.length < 8 && word.length > 3) 1 else if(word.length >= 8) 3 else 0
      fsm.levenshteinFiniteStateMachine(word, degree).toDfsm()
    })
  }



  def labelForText(label: String, dfsmList: List[DeterministicFiniteStateMachine]): Boolean = {
    //println(label)
    dfsmList.exists((dfsm: DeterministicFiniteStateMachine) => dfsm.isInDistance(label))
    //levenshteinDistanceClient.post("/labelForText/"+label, Json.listToJson(text))
  }

}

