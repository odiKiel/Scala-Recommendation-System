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

  override def apply(port: Int, name: String): Server = {
    super.apply(port, name)
  }

  def getService(): Service[HttpRequest, HttpResponse] = {
    val service = new Service[HttpRequest, HttpResponse] {
      def apply(request: HttpRequest) = {
        val r = new Promise[HttpResponse]
        val postregex = """[\?|&](\w+=\w+)""".r
        request.getMethod() match {
          case Method.Post => {
            val response = new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK)
            val value = request.getContent().toString("UTF-8").substring(1).split("&")
            val cb = ChannelBuffers.copiedBuffer(value(0).split("=")(1),Charset.defaultCharset())
            response.setHeader(HttpHeaders.Names.CONTENT_LENGTH, cb.readableBytes())
            response.setContent(cb)
            r.setValue(response)
            }
          case Method.Get => { 
            val response = new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.NOT_FOUND)
            val cb = ChannelBuffers.copiedBuffer("Hello World",Charset.defaultCharset())
            response.setHeader(HttpHeaders.Names.CONTENT_LENGTH, cb.readableBytes())
            response.setContent(cb)
            r.setValue(response)
          }
        }
        //val service = routing(request)

        //r.setContent(copiedBuffer("hello world", UTF_8))
        r
      }
    }
    service
  }

}
