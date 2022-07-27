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

case class BinaryTrace(url: String, clockUrl: String) extends ProgramTrace {
  private val file = new ChunkedFile(url)
  private val clockFile = new ChunkedFile(clockUrl)

  def read(
      from: Long,
      length: Int,
      forward: Boolean = true
  ): Future[Seq[Seq[TraceEvent]]] = {
    val t = System.currentTimeMillis()
    val clockFirst = (from - length) * BinaryTrace.IndexEntryBytes
    val clockLength = (length+1) * BinaryTrace.IndexEntryBytes
    clockFile.read(clockFirst, clockLength).flatMap { bytes =>
      val indexes = bytes.grouped(BinaryTrace.IndexEntryBytes).map(extractIndex).toSeq
      val indexesNew = if indexes.size < 2 then 0 +: indexes else indexes
      val ranges = indexesNew.init zip indexesNew.tail
      val futures = ranges.map((from, to) => file.read(from * BinaryTrace.EventBytes, (to - from) * BinaryTrace.EventBytes).map(s => s.grouped(BinaryTrace.EventBytes).map(extractEvent).toSeq))
      Future.sequence(futures.reverse)
    }
  }

  def length(): Future[Long] = clockFile.length().map(_ / BinaryTrace.IndexEntryBytes)

  private def extractEvent(s: Seq[Short]) =
    val clockBit = s(0) & (1 << 7)
    val address = (s(1) << 8) | s(2)

    s(0) match {
      case 0 => MemoryRead(address)
      case 1 => MemoryWrite(address)
    }

  private def extractIndex(s: Seq[Short]) =
    (s(0) << 24) | (s(1) << 16) | (s(2) << 8) | s(3)
}

object BinaryTrace {
  val EventBytes = 3
  val IndexEntryBytes = 4
}