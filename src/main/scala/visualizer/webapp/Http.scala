package visualizer.webapp

import org.scalajs.dom
import org.scalajs.dom.Blob
import scala.scalajs.js.Thenable.Implicits._
import scala.scalajs.js.typedarray._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/** Tools to make HTTP requests */
object Http {
  /** Fetches an entire text file 
   * 
   * @param url location of the resource
   * @return future that completes when a response is received and contains the text
  */
  def fetchText(url: String): Future[String] =
    dom.fetch(url).flatMap(r => r.text())

  /** Fetches a entire binary file
   * @param url location of the resource
   * @return future that completes when a response is received and contains the array of bytes
  */
  def fetchBinary(url: String): Future[Array[Short]] =
    dom
      .fetch(url)
      .flatMap(r => r.blob())
      .flatMap(b => b.arrayBuffer())
      .map(b => new Uint8Array(b).toArray)

  /** Fetches a subset of a binary file
   * @param url location of the resource
   * @param from index of the first byte to read
   * @param to index of the last byte to read
   * @return future that completes when a response is received and contains the array of bytes
  */
  def fetchBinary(url: String, from: Long, to: Long): Future[Array[Short]] =
    val h = new dom.Headers()
    h.set("Range", f"bytes=$from-$to")
    dom
      .fetch(url, new dom.RequestInit { headers = h })
      .flatMap(r => r.blob())
      .flatMap(b => b.arrayBuffer())
      .map(b => new Uint8Array(b).toArray)

  /** Performs a HTTP HEAD request, whose response contains only headers
   * 
   * @param url location of the resource
   * @return future that completes when a response is received and contains the headers
   */
  def fetchHead(url: String) =
    dom
      .fetch(url, new dom.RequestInit { method = dom.HttpMethod.HEAD })
      .map(r => r.headers)
}
