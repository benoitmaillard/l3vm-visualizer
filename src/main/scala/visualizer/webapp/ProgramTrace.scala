package visualizer.webapp

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

case class MemoryMetaData(regions: Seq[MemoryRegion], size: Int)
case class MemoryRegion(range: Range, name: String)

case class ProgramTrace(url: String, clockUrl: String, phaseUrl: String) {
  private val file = new TraceFile(ChunkedFile(url))
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

  def readRange(at: Long, lastN: Int): Future[Iterator[TraceEvent]] = {
    val tMin = math.max(0, at - lastN + 1)
    (clockFile.read(tMin) zip clockFile.read(at)) flatMap {
      case ((from, _), (_, to)) =>
        file.readRange(from, (to - from))
    }
  }

  def length() = clockFile.length()
}