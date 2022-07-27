package visualizer.webapp

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import scala.scalajs.js
import org.scalajs.dom
import js.Thenable.Implicits._
import scala.scalajs.js.Math.max
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js.typedarray._

import js.JSConverters._

case class BinaryTrace(url: String) extends ProgramTrace {
  private val file = new ChunkedFile(url)

  def read(
      from: Long,
      length: Int,
      forward: Boolean = true
  ): Future[Seq[TraceEvent]] = {
    val bytes = file.read((from - length + 1) * BinaryTrace.EventBytes, length * BinaryTrace.EventBytes)
    bytes.map(_.grouped(BinaryTrace.EventBytes).map(extractEvent).toSeq.reverse)
  }

  def length(): Future[Long] = file.length().map(_ / BinaryTrace.EventBytes)

  private def extractEvent(s: Seq[Short]) =
    val clockBit = s(0) & (1 << 7)
    val address = (s(1) << 8) | s(2)

    s(0) match {
      case 0 => MemoryRead(address)
      case 1 => MemoryWrite(address)
    }
}

object BinaryTrace {
  val EventBytes = 3
}