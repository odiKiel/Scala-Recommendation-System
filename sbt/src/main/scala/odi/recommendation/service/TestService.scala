package odi.recommendation
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

import com.twitter.finagle.Service

/** this is a minimalistic service for testing the network communication */
object TestService extends HttpServer {

  /** start the service
    * @param port of the service
    * @return the port that the service runs on
    */
  def apply(port: Int): Int = {
    super.apply(port, "testServer")
  }

  /** this method is called by the HttpServer router 
    * and forwards the request to the correct post method
    * @param path the path that the request is send to
    * @param value the value of the post body
    * @return it returns a future http request
    */
  def callPostMethod(path: Array[String], value: String): Future[HttpResponse] = {
    Future.value(
      path.head match {
        case "hello" => postHello(path.tail, value)
      }
    )
  }

  /** this method is called by the HttpServer router 
    * and forwards the request to the correct get method
    * @param path the path that the request is send to
    * @return it returns a future http request
    */
  def callGetMethod(path: Array[String]): Future[HttpResponse] = {
    Future.value(
      path.head match {
        case "hello" => getHello(path.tail)
        case "close" => getClose(path.tail)
      }
    )
  }

  def postHello(path: Array[String], value: String): HttpResponse = {
    createHttpResponse(value)
  }

  def getHello(path: Array[String]): HttpResponse = {
    createHttpResponse(path(0))
  }

  def getClose(path: Array[String]): HttpResponse = {
    super.close()
    createHttpResponse("Server is shutting down")
  }

}
