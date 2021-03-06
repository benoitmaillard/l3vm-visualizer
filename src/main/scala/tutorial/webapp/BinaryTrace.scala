package tutorial.webapp

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
  val chunkSize = 20000
  val nBytes = 3
  val chunkMap: mutable.Map[Long, Array[Short]] = mutable.Map()

  def read(
      from: Long,
      length: Int,
      forward: Boolean = true
  ): Future[Seq[TraceEvent]] = {
    val requiredChunks = (((from - length + 1) / chunkSize) to (from / chunkSize)).filter(_ >= 0)
    val futures = requiredChunks.map(fetchChunkIfNecessary)
    Future.sequence(futures).map { arrays =>
      val start = (from - requiredChunks(0) * chunkSize).toInt
      val joinedSeq = arrays.flatten.grouped(nBytes).slice((start - length + 1), start + 1).toSeq
      joinedSeq.map(extractEvent).toSeq.reverse
    }
  }

  def length(): Future[Long] =
    Http.fetchHead(url).map(h => h.get("content-length").toInt / nBytes)

  private def extractEvent(s: Seq[Short]) =
   
    val address = (s(1) << 8) |  s(2)

    s(0) match {
      case 0 => MemoryRead(address)
      case 1 => MemoryWrite(address)
    }

  private def fetchChunkIfNecessary(i: Long): Future[Array[Short]] =
    chunkMap.get(i).map(c => Future {c}).getOrElse(fetchChunk(i))

  private def fetchChunk(i: Long): Future[Array[Short]] =
    val first = chunkSize * i * nBytes
    val last = first + chunkSize * nBytes - 1
    Http.fetchBinary(url, first, last).map { a =>
      //println(a.toSeq.toString)

      chunkMap(i) = a
      a
    }
}
