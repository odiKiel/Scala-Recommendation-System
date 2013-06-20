package odi.recommendation
import org.jboss.netty.handler.codec.http.{HttpResponse}
import com.twitter.finagle.builder.Server
import com.twitter.util.{Promise, Future}


/** this service is responsible for the tagging of texts */
object TaggerService extends HttpServer {
  QueryStwServer(Services("queryStwServer").toInt)

  val queryStwClient = new HttpClient("localhost:"+Services("queryStwServer"))
  //val taggerPool = FuturePool(Executors.newFixedThreadPool(4))

  val name = "TaggerService"

  /** start the service
    * @param port of the service
    * @return the port that the service runs on
    */
  def apply(port: Int): Int = {
    super.apply(port, name)
  }

  /** this method is called by the HttpServer router 
    * and forwards the request to the correct post method
    * @param path the path that the request is send to
    * @param value the value of the post body
    * @return it returns a future http request
    */
  def callPostMethod(path: Array[String], value: String): Future[HttpResponse] = {
    path.head match {
      case "tagText" => postTagText(path.tail, value)  //returns the tags for a text
      case "prefLabelText" => postPrefLabelText(path.tail, value) //returns the preferred labels for a text
      case "tagsPrefLabels" => postTagsPrefLabels(path.tail, value)  //returns the tags for the preferred labels
      case "prefLabelsTags" => postPrefLabelsTags(path.tail, value) //ruturns the preferred labels for the tags
      case _ => Future.value(createHttpResponse("No such method"))
    }
  }

  /** this method is called by the HttpServer router 
    * and forwards the request to the correct get method
    * @param path the path that the request is send to
    * @return it returns a future http request
    */
  def callGetMethod(path: Array[String]): Future[HttpResponse] = {
    path.head match {
      case _ => Future.value(createHttpResponse("No such method"))
    }
  }

  /** return the tags for a text
    * 
    * @param args the path of the request
    * @param value the text that should be tagged
    * @return a list of tags as a httpresponse
    */
  def postTagText(args: Array[String], value: String): Future[HttpResponse] = {
    println(value)
    val r = new Promise[HttpResponse]
    tagText(value) onSuccess { tags => 
      println(tags)
      r.setValue(createHttpResponse(Json.toJson(tags.toList)))
    }
    r
  }


  /** return descriptors that correspond with the preferred label of the text
    *
    * @param args the path of the request
    * @param value the text that should be used
    * @return a list of descriptors as a http request
    */
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

  /** return the descriptor for the preferred labels that correspond with the tags
    *
    * @param args the path of the request
    * @param value a json string with a list of tags
    * @return a list of descriptors
    */
  def postPrefLabelsTags(args: Array[String], value: String): Future[HttpResponse] = {
    val tags = Json.jsonToList(value)
    val prefLabels = generatePrefLabelForTags(tags)
    Future.value(createHttpResponse(Json.toJson(prefLabels)))
  }

  /** return the tags that correspond with the descriptor of the preferred labels
    *
    * @param args the path of the request
    * @param value the json string with the descriptor
    */
  def postTagsPrefLabels(args: Array[String], value: String): Future[HttpResponse] = {

    val tags = Json.jsonToList(value)
    val prefLabels = generateTagForPrefLabels(tags)
  
      //r.setValue(createHttpResponse(Json.toJson(Future.collect(prefLabelFuture).get())))
      Future.value(createHttpResponse(Json.toJson(prefLabels))) // speed hack only words if TaggerService and QuerySTw run on the same server

  }

  /** returns a list of tags for a list of descriptors
    *
    * @param prefLabels a list of descriptors that correspond with the preferred labels
    * @return a list of tags
    */
  def generateTagForPrefLabels(prefLabels: List[String]): List[String] = {
    prefLabels.flatMap(prefLabel => {
        QueryStwServer.findTagByPrefLabel(prefLabel)
      }).distinct
  }

  /** return the descriptors that correspond with the preferred labels of the tags
    *
    * @param tags a list of tags
    * @return a list of descriptors that correspond with the tags
    */
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

  val query = "PREFIX skos: <http://www.w3.org/2004/02/skos/core#> PREFIX xml: <http://zbw.eu/stw/> SELECT ?q WHERE { ?s ?p ?q FILTER(?p = skos:prefLabel || ?p = skos:altLabel)} ORDER BY ?q"
  val pagination = 1000

  /** return the tags for a text
    *
    * @param text the text that should be tagged
    * @return a sequence of tags
    */
  def tagText(text: String): Future[Seq[String]] = {
    val time = System.nanoTime
    val textList = text.split("\\W").toList
    if(!(query contains "ORDER BY")) {
     throw new IllegalArgumentException("query needs to use ORDER BY")
    }


    val dfsmList = createDfsm(textList) // create a levenshtein automaton for each word

    tagTextRec(dfsmList, pagination, 0)

  }

  /** tag the text recursive 1000 words from the stw at the same time
    *
    * @param dfsmList a list of levenshtein automatos that are used for the tagging process
    * @param pagination the amount of stw words that should be used at the same time
    * @param offset the current offset
    */
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

  /** create a levenshtein automaton for each word in the list
    *
    * @param text the text that should be used
    * @return list of levenshtein automata
    */
  def createDfsm(text: List[String]): List[DeterministicFiniteStateMachine] = {
    text.map((word: String) => {
      val fsm = new FiniteStateMachine()
      val degree = if(word.length < 8 && word.length > 3) 1 else if(word.length >= 8) 3 else 0
      fsm.levenshteinFiniteStateMachine(word, degree).toDfsm()
    })
  }



  /** check if the label is in distance for all automata in the dfsm list
    *
    * @param label the word that should be tested as tag
    * @param dfsmList the list of automata
    */
  def labelForText(label: String, dfsmList: List[DeterministicFiniteStateMachine]): Boolean = {
    dfsmList.exists((dfsm: DeterministicFiniteStateMachine) => dfsm.isInDistance(label))
  }

}

