package odi.recommendation

import com.twitter.finagle.Service
import com.twitter.finagle.http.Http
import com.twitter.util.{Promise, Future}
import org.jboss.netty.handler.codec.http.{HttpResponseStatus, DefaultHttpResponse, DefaultHttpRequest, HttpRequest, HttpResponse, HttpVersion, HttpMethod, HttpHeaders, CookieDecoder, Cookie}
import java.net.{SocketAddress, InetSocketAddress}
import com.twitter.finagle.builder.{Server, ServerBuilder}
import com.twitter.finagle.builder.ClientBuilder
import com.twitter.finagle.builder.ServerBuilder
import com.twitter.finagle.http.Method
import org.jboss.netty.buffer.ChannelBuffers
import org.jboss.netty.buffer.ChannelBuffer
import java.nio.charset.Charset
import scala.collection.JavaConversions._

import scala.reflect._ //beanproperty


trait HttpServer {


  /** returns a finagle service for the server 
    *
    * this service defines the http request and http respond and forwards the request to the router
    */
  def getService(): Service[HttpRequest, HttpResponse] = {
    new Service[HttpRequest, HttpResponse] {
      def apply(request: HttpRequest): Future[HttpResponse] = {
        val r = new Promise[HttpResponse]
        val value = request.getHeader("Cookie")
        if(value != null) {
          cookies = new CookieDecoder().decode(value).toSet;
        }
      
        val responseFuture: Future[HttpResponse] = routing(request) 
        responseFuture onSuccess { response => 
          r.setValue(response)
        }
        r
      }
    }
  }


  var cookies = Set[Cookie]() // the cookie of the request if it exists
  var portServer = 0
  var running = false
  @BeanProperty // generates getServer() setServer() methods
  var server: Server = null

  def close() = {
    server.close()
  }

  /** start the service
    *
    * @param port the port on which the service should wait for a request
    * @param name the name of the service
    */
  def apply(port: Int, name: String): Int = {
    if(running) {
      return portServer
    }
    running = true
    portServer = port

    val address: SocketAddress = new InetSocketAddress(port)
    val current_server: Server = ServerBuilder()
      .codec(Http())
      .bindTo(address)
      .name(name)
      .build(getService())
    server = current_server
    port
  }


  /** the routing method this method decomposes the request
    * additionally it sends the name of the requested method to a post or get method forwarder
    * these forwarder must be implemented by the service
    */
  def routing(request: HttpRequest): Future[HttpResponse] = {
    //println("new request: "+request)
    val path = request.getUri().substring(1).split("/") // remove leading / and split
    request.getMethod() match {
      case Method.Post => callPostMethod(path, request.getContent().toString("UTF-8"))
      case Method.Get => callGetMethod(path)
    }

  }

  /** creates a http response for a sting
    *
    * @param value the string that should be included in the response
    * @return the http response 
    */
  def createHttpResponse(value: String): HttpResponse = {
    val response = new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK)
    val cb = ChannelBuffers.copiedBuffer(value,Charset.defaultCharset())
    response.setHeader(HttpHeaders.Names.CONTENT_LENGTH, cb.readableBytes())
    response.setContent(cb)
    response
  }

  /** creates an error http response that expresses that the method does not exist */
  def createErrorHttpResponse: HttpResponse = {
    new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.NOT_FOUND)
  }


  /** method definitions that must be implemented by the service */
  def callPostMethod(path: Array[String], value: String): Future[HttpResponse] 
  def callGetMethod(path: Array[String]): Future[HttpResponse] 
}


