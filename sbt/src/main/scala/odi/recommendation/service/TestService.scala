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

object TestService extends HttpServer {

  def apply(port: Int): Int = {
    super.apply(port, "testServer")
  }
  def callPostMethod(path: Array[String], value: String): Future[HttpResponse] = {
    Future.value(
      path.head match {
        case "hello" => postHello(path.tail, value)
      }
    )
  }

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
