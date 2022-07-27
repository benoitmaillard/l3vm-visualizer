package visualizer.webapp

import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class ChunkedFile(url: String) {
  private val chunkMap: mutable.Map[Long, Array[Short]] = mutable.Map()

  def read(from: Long, length: Int): Future[Seq[Short]] = {
    val requiredChunks = ((from / ChunkedFile.ChunkSize) to ((from + length - 1) / ChunkedFile.ChunkSize)).filter(_ >= 0)
    val futures = requiredChunks.map(fetchChunkIfNecessary)
    Future.sequence(futures).map { arrays =>
      val start = (from - requiredChunks(0) * ChunkedFile.ChunkSize).toInt
      val joinedSeq = arrays.flatten.slice(start, start + length).toSeq
      joinedSeq.toSeq
    }
  }

  def length(): Future[Long] =
    Http.fetchHead(url).map(h => h.get("content-length").toInt)

  private def fetchChunkIfNecessary(i: Long): Future[Array[Short]] =
    chunkMap.get(i).map(c => Future {c}).getOrElse(fetchChunk(i))

  private def fetchChunk(i: Long): Future[Array[Short]] =
    val first = ChunkedFile.ChunkSize * i
    val last = first + ChunkedFile.ChunkSize - 1
    Http.fetchBinary(url, first, last).map { a =>
      chunkMap(i) = a
      a
    }
}

object ChunkedFile {
  val ChunkSize = 0x80000
}
