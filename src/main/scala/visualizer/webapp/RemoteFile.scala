package visualizer.webapp

import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.View

trait RemoteFile[A] {
  def length(): Future[Long]
  def read(i: Long): Future[A]
  def readRange(from: Long, n: Int): Future[Iterator[A]]
}

class ChunkedFile(url: String) extends RemoteFile[Short] {
  private val chunkMap: mutable.Map[Long, Future[Array[Short]]] = mutable.Map()

  override def read(i: Long) =
    val requiredChunk = i / ChunkedFile.ChunkSize
    fetchChunkIfNecessary(i).map(a => a((i % ChunkedFile.ChunkSize).toInt))

  override def readRange(from: Long, n: Int) = {
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

  override def length(): Future[Long] =
    Http.fetchHead(url).map(h => h.get("content-length").toInt)

  private def fetchChunkIfNecessary(i: Long): Future[Array[Short]] =
    chunkMap.get(i).getOrElse(fetchChunk(i))

  private def fetchChunk(i: Long): Future[Array[Short]] =
    val first = ChunkedFile.ChunkSize * i
    val last = first + ChunkedFile.ChunkSize - 1
    val future = Http.fetchBinary(url, first, last)
    chunkMap(i) = future
    future
}

object ChunkedFile {
  val ChunkSize = 0x80000
}

class GroupedFile[A](file: RemoteFile[A], groupSize: Int) extends RemoteFile[Seq[A]] {
  override def length(): Future[Long] =
    file.length().map(_ / groupSize)

  override def read(i: Long): Future[Seq[A]] =
    file.readRange(i * groupSize, groupSize).map(_.toSeq)

  override def readRange(from: Long, n: Int) =
    file.readRange(from * groupSize, n * groupSize).map(_.grouped(groupSize))
}

// TODO not necessary ChunkedFile, any file with Short
class TraceIndex(file: ChunkedFile) extends RemoteFile[(Int, Int)] {
  private val groupedFile = GroupedFile(file, TraceIndex.EntryBytes)

  override def length(): Future[Long] = groupedFile.length()

  override def readRange(from: Long, length: Int): Future[Iterator[(Int, Int)]] = {
    val rangeStart = if from > 0 then from - 1 else 0
    val rangeLength = if from > 0 then length + 1 else length
    groupedFile.readRange(rangeStart, rangeLength) map { bytes =>
      val extracted = bytes.map(extractIndex)
      val complete = if from > 0 then extracted else Iterator(0) ++ extracted
      complete.sliding(2).withPartial(false).map(a => (a(0), a(1)))
    }
  }

  override def read(i: Long) =
    if i == 0 then groupedFile.read(0)
      .map(s => (0, extractIndex(s)))
    else groupedFile.readRange(i - 1, 2)
      .map(it => (extractIndex(it.next()), extractIndex(it.next())))

  private def extractIndex(s: Seq[Short]) =
    (s(0) << 24) | (s(1) << 16) | (s(2) << 8) | s(3)
}

object TraceIndex {
  val EntryBytes = 4
}

class PhaseIndex(file: ChunkedFile) extends RemoteFile[(TracePhase, Int, Int)] {
  private val groupedFile = GroupedFile(file, PhaseIndex.EntryBytes)

  override def length(): Future[Long] = groupedFile.length()

  override def readRange(from: Long, n: Int) =
    groupedFile.readRange(from, n).map(_.map(extractBounds))

  override def read(i: Long) =
    groupedFile.read(i).map(extractBounds)

  private def extractBounds(s: Seq[Short]) = (
    TracePhase.values(s(0)),
    (s(1) << 24) | (s(2) << 16) | (s(3) << 8) | s(4),
    (s(5) << 24) | (s(6) << 16) | (s(7) << 8) | s(8)
  )
}

abstract class TraceEvent
case class MemoryRead(address: Int) extends TraceEvent
case class MemoryWrite(address: Int) extends TraceEvent
case class PhaseStart(phase: TracePhase) extends TraceEvent
case class PhaseEnd(phase: TracePhase) extends TraceEvent

enum TracePhase(str: String) {
  override def toString(): String = str

  case CodeLoad extends TracePhase("Code load")
  case CodeExecute extends TracePhase("Code execute")
  case GarbageMark extends TracePhase("Garbage mark")
  case GarbageSweep extends TracePhase("Garbage sweep")
}

class TraceFile(file: ChunkedFile) extends RemoteFile[TraceEvent] {
  private val groupedFile = GroupedFile(file, PhaseIndex.EntryBytes)

  override def length(): Future[Long] = groupedFile.length()

  override def readRange(from: Long, n: Int) =
    groupedFile.readRange(from, n).map(_.map(extractEvent))

  override def read(i: Long) =
    groupedFile.read(i).map(extractEvent)

  private def extractEvent(s: Seq[Short]) = {
    val argument = (s(1) << 8) | s(2)

    s(0) & 0x7F match {
      case 0 => MemoryRead(argument)
      case 1 => MemoryWrite(argument)
      case 2 => PhaseStart(TracePhase.values(argument))
      case 3 => PhaseEnd(TracePhase.values(argument))
    }
  }
}

object PhaseIndex {
  val EntryBytes = 3
}