package odi.recommendation

import com.twitter.util.Duration
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
    val client = new HttpClient("localhost:10000")
    val ret1 = client.get("/")
    val ret2 = client.post("/", "echo->world")
    val ret3 = Future.collect(Seq(ret1, ret2))
    val value = ret3.get()
    server.close()
    "return OK after a get request" in {
      value(0) mustEqual ("Hello World")
    }

    "return echo after a post request" in {
      value(1) must startWith ("world")
    }


  }

}
