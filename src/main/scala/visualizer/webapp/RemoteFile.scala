package visualizer.webapp

import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.View

/** Abstract representation of a remote resource 
 * 
 * @tparam A type of elements that can be read 
*/
trait RemoteFile[A] {

  /** Length of the file (number of elements of type A that can be read) */
  def length(): Future[Long]

  /** Reads a single element 
   * 
   * @param i index of the element
  */
  def read(i: Long): Future[A]

  /** Reads a range of elements 
   * 
   * @param from index of first element
   * @param n number of elements to read
  */
  def readRange(from: Long, n: Int): Future[Iterator[A]]
}

/** File that is read chunk by chunk from the remote server. Chunks
 *  are downloaded when needed in a manner that is transparent to the
 *  user. Once downloaded, a chunk is kept in memory.
 * 
 * @param url location of the resource
 * 
 */
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

/** File that is always read by groups of n elements (n is fixed). Indexes
 *  used when reading from such a file are indexes of groups and
 *  not of individual elements.
 * 
 * @tparam A type of elements in the underlying file
 * @param file underlying file
 * @param groupSize size of a group
*/
class GroupedFile[A](file: RemoteFile[A], groupSize: Int) extends RemoteFile[Seq[A]] {
  override def length(): Future[Long] =
    file.length().map(_ / groupSize)

  override def read(i: Long): Future[Seq[A]] =
    file.readRange(i * groupSize, groupSize).map(_.toSeq)

  override def readRange(from: Long, n: Int) =
    file.readRange(from * groupSize, n * groupSize).map(_.grouped(groupSize))
}

/** Index file that contains a mapping from clock ticks to indexes of events
 *  in the program trace. Such an index file is represented in binary
 *  as follows: an integer at position i in the index file is the end
 *  (position of the last event + 1) of the range for tick i.
 * 
 * @param file underlying file
 */
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

/** Index file that contains data about the phases in the program execution.
 *  Each phase is represented as 9 bytes: byte 0 is the type of phase,
 *  bytes 1-4 are the beginning (in clock time) and bytes 5-8 are the
 *  end of the phase.
 * 
 * @param file underlying file
 */
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

object PhaseIndex {
  val EntryBytes = 9
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

/** File that contains the event data. Each event is represented
 *  as 3 bytes: byte 0 is the type of event, bytes 1-2 are
 *  the word address.
 * 
 * @param file underlying file
 */
class TraceFile(file: ChunkedFile) extends RemoteFile[TraceEvent] {
  private val groupedFile = GroupedFile(file, TraceFile.EntryBytes)

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

object TraceFile {
  val EntryBytes = 3
}