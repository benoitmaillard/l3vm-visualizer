package tutorial.webapp

import org.scalajs.dom
import org.scalajs.dom.Blob
import scala.scalajs.js.Thenable.Implicits._
import scala.scalajs.js.typedarray._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object Http {
  def fetchText(url: String): Future[String] =
    dom.fetch(url).flatMap(r => r.text())

  def fetchBinary(url: String): Future[Array[Short]] =
    dom
      .fetch(url)
      .flatMap(r => r.blob())
      .flatMap(b => b.arrayBuffer())
      .map(b => new Uint8Array(b).toArray)

  def fetchBinary(url: String, from: Long, to: Long): Future[Array[Short]] =
    val h = new dom.Headers()
    h.set("Range", f"bytes=$from-$to")
    dom
      .fetch(url, new dom.RequestInit { headers = h })
      .flatMap(r => r.blob())
      .flatMap(b => b.arrayBuffer())
      .map(b => new Uint8Array(b).toArray)

  def fetchHead(url: String) =
    dom
      .fetch(url, new dom.RequestInit { method = dom.HttpMethod.HEAD })
      .map(r => r.headers)
}
