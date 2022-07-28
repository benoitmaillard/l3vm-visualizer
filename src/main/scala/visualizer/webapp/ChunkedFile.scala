package visualizer.webapp

import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.View

class ChunkedFile(url: String) {
  private val chunkMap: mutable.Map[Long, Future[Array[Short]]] = mutable.Map()

  def read(from: Long, n: Int): Future[Iterator[Short]] = {
    val requiredChunks = ((from / ChunkedFile.ChunkSize) to ((from + n - 1) / ChunkedFile.ChunkSize)).filter(_ >= 0)
    val futures = requiredChunks.map(fetchChunkIfNecessary)
    Future.sequence(futures).map { arrays =>
      val start = (from - requiredChunks(0) * ChunkedFile.ChunkSize).toInt
      new Iterator[Short] {
        var i = start
        def hasNext = i < start + n
        def next(): Short = {
          val arrayIndex = i / ChunkedFile.ChunkSize
          val internalIndex = i % ChunkedFile.ChunkSize
          i += 1
          arrays(arrayIndex)(internalIndex)
        }
      }
    }
  }

  def length(): Future[Long] =
    Http.fetchHead(url).map(h => h.get("content-length").toInt)

  private def fetchChunkIfNecessary(i: Long): Future[Array[Short]] =
    chunkMap.get(i).getOrElse(fetchChunk(i))

  private def fetchChunk(i: Long): Future[Array[Short]] =
    val first = ChunkedFile.ChunkSize * i
    val last = first + ChunkedFile.ChunkSize - 1
    println("fetch!")
    val future = Http.fetchBinary(url, first, last)
    chunkMap(i) = future
    future
}

object ChunkedFile {
  val ChunkSize = 0x80000
}
