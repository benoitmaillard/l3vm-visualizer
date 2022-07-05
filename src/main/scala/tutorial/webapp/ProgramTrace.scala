package tutorial.webapp

import scala.concurrent.Future

abstract class TraceEvent
case class MemoryRead(address: Int) extends TraceEvent
case class MemoryWrite(address: Int) extends TraceEvent

trait ProgramTrace {
  def read(
      at: Int,
      last: Int,
      forward: Boolean = true
  ): Future[Seq[TraceEvent]]
  def length(): Future[Int]
}
