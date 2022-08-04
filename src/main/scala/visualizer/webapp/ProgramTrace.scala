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

case class ProgramTrace(url: String, clockUrl: String, phaseUrl: String) {
  private val file = new GroupedFile(ChunkedFile(url), ProgramTrace.EventBytes)
  private val clockFile = new TraceIndex(ChunkedFile(clockUrl))
  private val phaseFile = new PhaseIndex(ChunkedFile(phaseUrl))

  def phases(): Future[Iterator[(TracePhase, Int, Int)]] =
    phaseFile.length().flatMap(l => phaseFile.readRange(0, l.toInt))

  // def read(
  //     from: Long,
  //     length: Int,
  //     forward: Boolean = true
  // ): Future[Seq[Seq[TraceEvent]]] = {
  //   val tMin = math.max(0, from - length + 1)
  //   val tLength = math.min(from + 1, length).toInt
    
  //   clockFile.readAll(tMin, tLength).flatMap { ranges =>
  //     val futures = ranges.map((from, to) => file.read(from, (to - from), BinaryTrace.EventBytes).map(s => s.map(extractEvent).toSeq))
  //     Future.sequence(futures.toSeq.reverse)
  //   }
  // }

  def readRange(from: Long, length: Int): Future[Iterator[TraceEvent]] = {
    val tMin = math.max(0, from - length + 1)
    (clockFile.read(tMin) zip clockFile.read(from)) flatMap {
      case ((from, _), (_, to)) =>
        file.readRange(from, (to - from)).map(_.map(extractEvent))
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

object ProgramTrace {
  val EventBytes = 3
}
