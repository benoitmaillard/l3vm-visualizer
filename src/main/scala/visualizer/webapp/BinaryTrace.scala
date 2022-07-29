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

class TraceIndex(clockUrl: String) {
  private val file = new ChunkedFile(clockUrl)

  def readLast(from: Long, length: Int): Future[Iterator[(Int, Int)]] = {
    val rangeStart = if from > 0 then from - 1 else 0
    val rangeLength = if from > 0 then length + 1 else length
    file.read(rangeStart, rangeLength, TraceIndex.EntryBytes) map { bytes =>
      val extracted = bytes.map(extractIndex)
      val complete = if from > 0 then extracted else Iterator(0) ++ extracted
      complete.sliding(2).withPartial(false).map(a => (a(0), a(1)))
    }
  }

  def length(): Future[Long] = file.length().map(_ / TraceIndex.EntryBytes)

  private def extractIndex(s: Seq[Short]) =
    (s(0) << 24) | (s(1) << 16) | (s(2) << 8) | s(3)
}

object TraceIndex {
  val EntryBytes = 4
}

case class BinaryTrace(url: String, clockUrl: String) extends ProgramTrace {
  private val file = new ChunkedFile(url)
  private val clockFile = new TraceIndex(clockUrl)

  def read(
      from: Long,
      length: Int,
      forward: Boolean = true
  ): Future[Seq[Seq[TraceEvent]]] = {
    val tMin = math.max(0, from - length + 1)
    val tLength = math.min(from + 1, length).toInt
    clockFile.readLast(tMin, tLength).flatMap { ranges =>
      val futures = ranges.map((from, to) => file.read(from, (to - from), BinaryTrace.EventBytes).map(s => s.map(extractEvent).toSeq))
      Future.sequence(futures.toSeq.reverse)
    }
  }

  def length() = clockFile.length()

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