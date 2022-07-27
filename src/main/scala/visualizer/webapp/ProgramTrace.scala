package visualizer.webapp

import scala.concurrent.Future

case class MemoryMetaData(regions: Seq[MemoryRegion], size: Int)
case class MemoryRegion(range: Range, name: String)

abstract class TraceEvent
case class MemoryRead(address: Int) extends TraceEvent
case class MemoryWrite(address: Int) extends TraceEvent
case class PhaseStart(phase: TracePhase) extends TraceEvent
case class PhaseEnd(phase: TracePhase) extends TraceEvent

abstract class TracePhase
case object CodeLoad extends TracePhase
case object CodeExecute extends TracePhase
case object GarbageMark extends TracePhase
case object GarbageSweep extends TracePhase

trait ProgramTrace {
  def read(
      at: Long,
      last: Int,
      forward: Boolean = true
  ): Future[Seq[Seq[TraceEvent]]]
  def length(): Future[Long]
}
