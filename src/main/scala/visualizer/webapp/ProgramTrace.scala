package visualizer.webapp

import scala.concurrent.Future

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


trait ProgramTrace {
  def read(
      at: Long,
      last: Int,
      forward: Boolean = true
  ): Future[Seq[Seq[TraceEvent]]]
  def readBulk(from: Long, length: Int): Future[Iterator[TraceEvent]]
  def length(): Future[Long]
}
