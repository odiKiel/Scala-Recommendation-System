package odi.recommendation

import org.specs2.mutable._
import org.jboss.netty.handler.codec.http.{HttpResponseStatus, DefaultHttpResponse, DefaultHttpRequest, HttpRequest, HttpResponse, HttpVersion, HttpMethod, HttpHeaders}
import com.twitter.finagle.Service
import com.twitter.finagle.builder.ClientBuilder
import com.twitter.finagle.http.Http
import com.twitter.util.{Future,Promise}
import org.jboss.netty.buffer.ChannelBuffers
import java.nio.charset.Charset


object HttpServerSpec extends Specification {
  "A running httpServer" should {
    val server = HttpServer()
    val client: Service[HttpRequest, HttpResponse] = ClientBuilder()
      .codec(Http())
      .hosts("localhost:10000") // If >1 host, client does simple load-balancing
      .hostConnectionLimit(1)
      .build()

    
    "return OK after a get request" in {

      val req = new DefaultHttpRequest(HttpVersion.HTTP_1_1, HttpMethod.GET, "/")

      val f = client(req) // Client, send the request
      
      var status = new Promise[String]
      f onSuccess { res =>
        status.setValue(res.getStatus().toString())
      } onFailure { exc =>
        status.setException(exc)
      }

      status.get() must startWith ("200 OK")
    }

    "should return a desciptor after a post request" in {

      var httpReq = new DefaultHttpRequest(HttpVersion.HTTP_1_1,HttpMethod.POST,"/")
      //httpReq.setHeader(HttpHeaders.Names.HOST,"localhost:10000")
      //httpReq.setHeader(HttpHeaders.Names.CONNECTION,HttpHeaders.Values.KEEP_ALIVE)
      //httpReq.setHeader(HttpHeaders.Names.ACCEPT_ENCODING,HttpHeaders.Values.GZIP)
      //httpReq.setHeader(HttpHeaders.Names.CONTENT_TYPE,"application/x-www-form-urlencoded")
      val params="tag=Hanf"
      val cb = ChannelBuffers.copiedBuffer(params,Charset.defaultCharset())
      httpReq.setHeader(HttpHeaders.Names.CONTENT_LENGTH,cb.readableBytes())
      httpReq.setContent(cb)
      
      val f = client(httpReq)
      
      var status = Promise[String]
      f onSuccess { res =>
        status.setValue(res.getStatus.toString())
      } onFailure { exc =>
        status.setException(exc)
      }
      server.close()
      status.get() mustEqual ("19017-2")
    }

  }

}
