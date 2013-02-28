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
import org.jboss.netty.buffer.ChannelBuffers
import org.jboss.netty.buffer.ChannelBuffer
import java.nio.charset.Charset


trait HttpServer {


  def getService(): Service[HttpRequest, HttpResponse]

  def apply(port: Int, name: String): Server = {
    val address: SocketAddress = new InetSocketAddress(port)
    val server: Server = ServerBuilder()
      .codec(Http())
      .bindTo(address)
      .name(name)
      .build(getService())
    server
  }
/*
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
  */
}


