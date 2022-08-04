package visualizer.webapp

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

case class MemoryMetaData(regions: Seq[MemoryRegion], size: Int)
case class MemoryRegion(range: Range, name: String)

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


class TraceIndex(clockUrl: String) {
  private val file = new ChunkedFile(clockUrl)

  def readAll(from: Long, length: Int): Future[Iterator[(Int, Int)]] = {
    val rangeStart = if from > 0 then from - 1 else 0
    val rangeLength = if from > 0 then length + 1 else length
    file.read(rangeStart, rangeLength, TraceIndex.EntryBytes) map { bytes =>
      val extracted = bytes.map(extractIndex)
      val complete = if from > 0 then extracted else Iterator(0) ++ extracted
      complete.sliding(2).withPartial(false).map(a => (a(0), a(1)))
    }
  }

  def readRange(from: Long, length: Int): Future[(Int, Int)] = {
    val startFuture = if from == 0 then Future {0} else file.read(from - 1, 1, TraceIndex.EntryBytes).map(c => extractIndex(c.next()))
    val endFuture = file.read((from + length - 1), 1, TraceIndex.EntryBytes).map(c => extractIndex(c.next()))
    if from == 0 then endFuture.map(r => (0, r))
    else (startFuture zip endFuture)
  }

  def length(): Future[Long] = file.length().map(_ / TraceIndex.EntryBytes)

  private def extractIndex(s: Seq[Short]) =
    (s(0) << 24) | (s(1) << 16) | (s(2) << 8) | s(3)
}

object TraceIndex {
  val EntryBytes = 4
}

class PhaseIndex(url: String) {
  val file = ChunkedFile(url)

  def length(): Future[Long] = file.length().map(_ / PhaseIndex.EntryBytes)

  def read(from: Long, length: Int): Future[Iterator[(TracePhase, Int, Int)]] = 
    file.read(from, length, PhaseIndex.EntryBytes).map(_.map(extractBounds))

  private def extractBounds(s: Seq[Short]) = (
    TracePhase.values(s(0)),
    (s(1) << 24) | (s(2) << 16) | (s(3) << 8) | s(4),
    (s(5) << 24) | (s(6) << 16) | (s(7) << 8) | s(8)
  )
}

object PhaseIndex {
  val EntryBytes = 9
}

case class ProgramTrace(url: String, clockUrl: String, phaseUrl: String) {
  private val file = new ChunkedFile(url)
  private val clockFile = new TraceIndex(clockUrl)
  private val phaseFile = new PhaseIndex(phaseUrl)

  def phases(): Future[Iterator[(TracePhase, Int, Int)]] =
    phaseFile.length().flatMap(l => phaseFile.read(0, l.toInt))

  def read(
      from: Long,
      length: Int,
      forward: Boolean = true
  ): Future[Seq[Seq[TraceEvent]]] = {
    val tMin = math.max(0, from - length + 1)
    val tLength = math.min(from + 1, length).toInt
    
    clockFile.readAll(tMin, tLength).flatMap { ranges =>
      val futures = ranges.map((from, to) => file.read(from, (to - from), BinaryTrace.EventBytes).map(s => s.map(extractEvent).toSeq))
      Future.sequence(futures.toSeq.reverse)
    }
  }

  def readBulk(from: Long, length: Int): Future[Iterator[TraceEvent]] = {
    val tMin = math.max(0, from - length + 1)
    val tLength = math.min(from + 1, length).toInt

    clockFile.readRange(tMin, tLength).flatMap { (from, to) =>
      file.read(from, (to - from), BinaryTrace.EventBytes).map(s => s.map(extractEvent))
    }
  }

  def length() = clockFile.length()

  private def extractEvent(s: Seq[Short]) =
    // val clockBit = ((s(0) & 0x80) >>> 7) == 1
    val argument = (s(1) << 8) | s(2)

    s(0) & 0x7F match {
      case 0 => MemoryRead(argument)
      case 1 => MemoryWrite(argument)
      case 2 => PhaseStart(TracePhase.values(argument))
      case 3 => PhaseEnd(TracePhase.values(argument))
    }
}

object BinaryTrace {
  val EventBytes = 3
}
