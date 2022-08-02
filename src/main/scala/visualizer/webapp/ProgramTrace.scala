package visualizer.webapp

import scala.concurrent.Future

case class MemoryMetaData(regions: Seq[MemoryRegion], size: Int)
case class MemoryRegion(range: Range, name: String)

abstract class TraceEvent
case class MemoryRead(address: Int) extends TraceEvent
case class MemoryWrite(address: Int) extends TraceEvent
case class PhaseStart(phase: TracePhase) extends TraceEvent
case class PhaseEnd(phase: TracePhase) extends TraceEvent

enum TracePhase:
    case CodeLoad extends TracePhase
    case CodeExecute extends TracePhase
    case GarbageMark extends TracePhase
    case GarbageSweep extends TracePhase

trait ProgramTrace {
  def read(
      at: Long,
      last: Int,
      forward: Boolean = true
  ): Future[Seq[Seq[TraceEvent]]]
  def readBulk(from: Long, length: Int): Future[Iterator[TraceEvent]]
  def length(): Future[Long]
}
