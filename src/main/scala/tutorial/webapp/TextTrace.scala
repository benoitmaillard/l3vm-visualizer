package tutorial.webapp

import scala.scalajs.js
import org.scalajs.dom
import js.Thenable.Implicits._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js.Math.max

case class TextTrace(url: String) extends ProgramTrace {
  private var content: Array[TraceEvent] = Array.empty

  private def parseLine(line: String): TraceEvent = line.toSeq match {
    case 'W' +: tail => MemoryRead(Integer.parseInt(tail.toString, 16))
    case 'R' +: tail => MemoryRead(Integer.parseInt(tail.toString, 16))
  }

  private def fetchIfRequired(): Future[Array[TraceEvent]] =
    if content.isEmpty then
      Http
        .fetchText(url)
        .map(f => {
          content = f.split("\n").map(parseLine)
          content
        })
    else Future { content }

  def read(at: Int, last: Int, forward: Boolean): Future[Seq[TraceEvent]] =
    fetchIfRequired().map(c => c.slice(at - last + 1, at + 1).reverse)

  def length(): Future[Int] =
    fetchIfRequired().map(c => c.length)
}
