package odi.recommendation

import org.specs2.mutable._
import org.jboss.netty.handler.codec.http.{HttpResponseStatus, DefaultHttpResponse, DefaultHttpRequest, HttpRequest, HttpResponse, HttpVersion, HttpMethod, HttpHeaders}
import com.twitter.finagle.Service
import com.twitter.finagle.builder.ClientBuilder
import com.twitter.finagle.http.Http
import com.twitter.util.TimeConversions._
import com.twitter.util.{Duration, Future, Promise, Time, Timer, Try}
import org.jboss.netty.buffer.ChannelBuffers
import org.jboss.netty.buffer.ChannelBuffer
import java.nio.charset.Charset

/** the http client that is used for communicating with the services */
class HttpClient(hosts: String) {
  val client: Service[HttpRequest, HttpResponse] = ClientBuilder()
     .codec(Http())
     .hosts(hosts) // If >1 host, client does simple load-balancing
     .tcpConnectTimeout(5.seconds)
     .requestTimeout(60.seconds)
     .hostConnectionLimit(1000)
     .logger(java.util.logging.Logger.getLogger("debug"))
     .retries(3)
     .build()
     

  /** post a request to a service 
    * 
    * @param path the path of the method of the service
    * @param params the params that should be provided
    * @return a future promise
    */
  def post(path: String, params: String): Promise[String] = {
    var httpReq = new DefaultHttpRequest(HttpVersion.HTTP_1_1,HttpMethod.POST,path)
    val cb = ChannelBuffers.copiedBuffer(params,Charset.defaultCharset())
    httpReq.setHeader(HttpHeaders.Names.CONTENT_LENGTH,cb.readableBytes())
    httpReq.setContent(cb)
    
    val f = client(httpReq)
    
    var status = Promise[String]
    f onSuccess { res =>
      status.setValue(res.getContent().toString("UTF-8"))
    } onFailure { exc =>
      status.setException(exc)
    }
    status

  }

  /** send a get request to a service
    *
    * @param path the path for the method of the service
    * @return a future promise 
    */
  def get(path: String): Promise[String] = {
    val req = new DefaultHttpRequest(HttpVersion.HTTP_1_1, HttpMethod.GET, path)
    req.setHeader(HttpHeaders.Names.HOST, hosts)
    req.setHeader(HttpHeaders.Names.CONNECTION, HttpHeaders.Values.CLOSE)
    req.setHeader(HttpHeaders.Names.ACCEPT_ENCODING, HttpHeaders.Values.GZIP)
    
    val f = client(req) // Client, send the request
    var status = new Promise[String]
    f onSuccess { res =>
      status.setValue(res.getContent().toString("UTF-8"))
    } onFailure { exc =>
      status.setException(exc)
    }
    status
  }

  def close() = {
    client.close()
  }
 
}
