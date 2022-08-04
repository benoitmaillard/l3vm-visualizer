package visualizer.webapp

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

case class MemoryMetaData(regions: Seq[MemoryRegion], size: Int)
case class MemoryRegion(range: Range, name: String)

/** Program trace
 * 
 * @param url location of the trace file
 * @param clockUrl location of the clock index file
 * @param phaseUrl location of the phase index file
 * 
*/
case class ProgramTrace(url: String, clockUrl: String, phaseUrl: String) {
  private val file = new TraceFile(ChunkedFile(url))
  private val clockFile = new TraceIndex(ChunkedFile(clockUrl))
  private val phaseFile = new PhaseIndex(ChunkedFile(phaseUrl))

  /** Retrieves the phases in the program trace. Each phase is represented
      by its type as well as by its starting and ending time.  
   * @return future with an iterator of the phases
   * 
  */
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
  
  /** Retrieves a range from the program trace. The range is defined
   *  by the current tick as well as the number of previous ticks
   *  we need to retrieve. Each tick might corresponds to any number of events.
   * 
   * @param at current time
   * @param lastN number of ticks in the range. If this parameter is 1, only the current
   *  tick is retrieved. If it is n, the current tick as well as the n-1 previous ticks
   *  are retrieved.
   * @return future that contains all the events that happened in the range
  */
  def readRange(at: Long, lastN: Int): Future[Iterator[TraceEvent]] = {
    val tMin = math.max(0, at - lastN + 1)
    (clockFile.read(tMin) zip clockFile.read(at)) flatMap {
      case ((from, _), (_, to)) =>
        file.readRange(from, (to - from))
    }
  }

  /** Number of clock ticks in the trace */
  def length() = clockFile.length()
}