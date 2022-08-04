package visualizer.webapp

import org.scalajs.dom
import scala.collection.mutable.HashMap

/** Tools to draw the program trace on a discrete grid  
 * 
 * @constructor create a new GridManager
 * @param painter canvas painter
 * @param memRep spatial memory representation to use for mapping on the grid
 * @param squareWidth width (and height) of a single square on the grid
 * @param metadata information about the memory size and regions 
*/
class GridManager(
    painter: CanvasPainter,
    memRep: SpatialMemoryRepresentation,
    squareWidth: Int,
    metadata: MemoryMetaData
) {
  val counts = HashMap[Int, (Int, Int)]()

  val boundaryWidth = 1
  val gridRect = GridRectangle(0, 0, memRep.height, memRep.width)
  painter.resize(memRep.width * squareWidth, memRep.height * squareWidth)
  val boundaries = metadata.regions.map(r => computeBoundaries(r.range))
  val colors = Color.range(GridManager.RegionPaletteFrom, GridManager.RegionPaletteTo, boundaries.length)
  (boundaries zip colors).foreach(drawBoundaries)
  painter.refresh(1)

  // def processEventSeq(s: Seq[Seq[TraceEvent]]): Unit = {
  //   s.zipWithIndex.reverse.foreach((c, i) => c.foreach(processEvent(_, i)))
  //   painter.refresh(0)
  // }
  
  /** Displays a serie of trace events on the grid. The intensity of the color
   *  on a square depends on the number of accesses on the corresponding word.
   *  The more accesses, the more intense the color. Write and read events
   *  are displayed with distinct colors. The colors are mixed when both
   *  read and write events are present.
   * 
   * @param events events to display
   * 
  */
  def processAll(events: Iterator[TraceEvent]): Unit = {
    counts.clear()
    events.foreach {
      case MemoryRead(address) => counts.updateWith(address)(v => Some(v.map((r, w) => (r + 1, w)).getOrElse((1, 0))))
      case MemoryWrite(address) => counts.updateWith(address)(v => Some(v.map((r, w) => (r, w + 1)).getOrElse((0, 1))))
      case _ =>
    }
    val maxCount = counts.values.max
    counts.foreach { case (address, (r, w)) =>
      val readRatio = if maxCount._1 == 0 then 0 else r.toFloat / maxCount._1
      val writeRatio = if maxCount._2 == 0 then 0 else w.toFloat / maxCount._2
      val total = readRatio + writeRatio
      if (total > 0) {
        val color = GridManager.ReadColor * (readRatio / total) + GridManager.WriteColor * (writeRatio / total)
        drawWord(address, color.copy(a = 0.5f + 0.5f * total))
      }
    }
    painter.refresh(0)
  }

  // private def processEvent(e: TraceEvent, pos: Int): Unit = e match {
  //   case MemoryRead(address) =>
  //     drawWord(address, GridManager.ReadColor.copy(a = alphaAt(pos)))
  //   case MemoryWrite(address) =>
  //     drawWord(address, GridManager.WriteColor.copy(a = alphaAt(pos)))
  // }

  // private def alphaAt(pos: Int) = 1.0f / (pos+1)

  private def drawWord(address: Int, color: Color): Unit = {
    val square = memRep.addressToSquare(address)
    drawSquare(square, color)
  }

  private def drawSquare(s: GridSquare, color: Color): Unit = {
    painter.drawRect(squareWidth * s.col, squareWidth * s.row, squareWidth, squareWidth, color, 0)
  }

  private def drawBoundaries(boundaries: Seq[(GridSquare, Orientation)], c: Color) =
    boundaries.foreach((s, o) => drawSingleBoundary(s, o, c))

  private def drawSingleBoundary(s: GridSquare, o: Orientation, c: Color) = {
    val w = if o.colDiff == 0 then squareWidth else boundaryWidth
    val h = if o.rowDiff == 0 then squareWidth else boundaryWidth
    val x = if o.colDiff == 1 then (s.col+1) * squareWidth - boundaryWidth else s.col * squareWidth
    val y = if o.rowDiff == 1 then (s.row+1) * squareWidth - boundaryWidth else s.row * squareWidth
    painter.drawRect(x, y, w, h, c, 1)
  }

  private def computeBoundaries(r: Range): Seq[(GridSquare, Orientation)] = {
    val rangeSquares = r.map(memRep.addressToSquare)
    for (
      s <- rangeSquares;
      (adj, or) <- s.allAdjacent if !gridRect.contains(adj) || !r.contains(memRep.squareToAddress(adj))
    ) yield (s, or)
  }
}

object GridManager {
  val ReadColor = Color(0.0f, 1.0f, 0.0f, 1.0f)
  val WriteColor = Color(1.0f, 0.0f, 0.0f, 1.0f)
  val RegionPaletteFrom = Color(0.97f, 0.78f, 0.05f, 1.0f)
  val RegionPaletteTo = Color(0.4f, 0.18f, 0.6f, 1.0f)
}
