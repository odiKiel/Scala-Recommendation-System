package odi.recommendation

import com.twitter.finagle.Service
import com.twitter.finagle.http.Http
import com.twitter.util.{Promise, Future}
import org.jboss.netty.handler.codec.http.{HttpResponseStatus, DefaultHttpResponse, DefaultHttpRequest, HttpRequest, HttpResponse, HttpVersion, HttpMethod, HttpHeaders}
import java.net.{SocketAddress, InetSocketAddress}
import com.twitter.finagle.builder.{Server, ServerBuilder}
import com.twitter.finagle.builder.ClientBuilder
import com.twitter.finagle.builder.ServerBuilder
import com.twitter.finagle.http.Method
import org.jboss.netty.buffer.ChannelBuffers
import org.jboss.netty.buffer.ChannelBuffer
import java.nio.charset.Charset

import scala.reflect._ //beanproperty


trait HttpServer {


  def getService(): Service[HttpRequest, HttpResponse] = {
    new Service[HttpRequest, HttpResponse] {
      def apply(request: HttpRequest): Future[HttpResponse] = {
        val r = new Promise[HttpResponse]
        val responseFuture: Future[HttpResponse] = routing(request) 
        responseFuture onSuccess { response => 
          r.setValue(response)
        }
        r
      }
    }
  }


  var portServer = 0
  var running = false
  @BeanProperty // generates getServer() setServer() methods
  var server: Server = null

  def close() = {
    server.close()
  }

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
  //val hosts = Map("updateService" -> "localhost:11000", "requestService" -> "localhost:12000")
  def routing(request: HttpRequest): Future[HttpResponse] = {
    val path = request.getUri().substring(1).split("/") // remove leading / and split
    request.getMethod() match {
      case Method.Post => callPostMethod(path, request.getContent().toString("UTF-8"))
      case Method.Get => callGetMethod(path)
    }

  }

  def createHttpResponse(value: String): HttpResponse = {
    val response = new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.NOT_FOUND)
    val cb = ChannelBuffers.copiedBuffer(value,Charset.defaultCharset())
    response.setHeader(HttpHeaders.Names.CONTENT_LENGTH, cb.readableBytes())
    response.setContent(cb)
    response
  }


  def callPostMethod(path: Array[String], value: String): Future[HttpResponse] 
  def callGetMethod(path: Array[String]): Future[HttpResponse] 
/*
  def getService(host: String): Service[HttpRequest, HttpResponse] = {
    ClientBuilder()
      .codec(Http())
      .hosts(host)
      .hostConnectionLimit(1)
      .build()
  }
  */
}


