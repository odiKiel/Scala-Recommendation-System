package odi.recommendation

import com.twitter.finagle.Service
import com.twitter.finagle.http.Http
import com.twitter.util.Future
import org.jboss.netty.handler.codec.http.{HttpResponseStatus, DefaultHttpResponse, DefaultHttpRequest, HttpRequest, HttpResponse, HttpVersion, HttpMethod, HttpHeaders}
import java.net.{SocketAddress, InetSocketAddress}
import com.twitter.finagle.builder.{Server, ServerBuilder}
import com.twitter.finagle.builder.ClientBuilder
import com.twitter.finagle.builder.ServerBuilder
import com.twitter.finagle.http.Method

object HttpServer {
  val rootService = new Service[HttpRequest, HttpResponse] {
    def apply(request: HttpRequest) = {
      val r = request.getUri match {
        case "/" => new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK)
        case _ => new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.NOT_FOUND)
      }
      //val service = routing(request)
      Future.value(r)
    }
  }

  def apply(): Server = {
    val address: SocketAddress = new InetSocketAddress(10000)
    val server: Server = ServerBuilder()
      .codec(Http())
      .bindTo(address)
      .name("HttpServer")
      .build(rootService)
    server
  }

  val hosts = Map("updateService" -> "localhost:11000", "requestService" -> "localhost:12000")
  def routing(request: HttpRequest): Service[HttpRequest, HttpResponse] = {
    request.getMethod() match {
      case Method.Post => getService(hosts("updateService"))
      case Method.Get => getService(hosts("requestService"))
    }

  }

  def getService(host: String): Service[HttpRequest, HttpResponse] = {
    ClientBuilder()
      .codec(Http())
      .hosts(host)
      .hostConnectionLimit(1)
      .build()
  }
}


