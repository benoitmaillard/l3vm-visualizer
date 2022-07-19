package tutorial.webapp

import scala.concurrent.Future

case class MemoryMetaData(regions: Seq[MemoryRegion], size: Int)
case class MemoryRegion(range: Range, name: String)

abstract class TraceEvent
case class MemoryRead(address: Int) extends TraceEvent
case class MemoryWrite(address: Int) extends TraceEvent

trait ProgramTrace {
  def read(
      at: Long,
      last: Int,
      forward: Boolean = true
  ): Future[Seq[TraceEvent]]
  def length(): Future[Long]
}
