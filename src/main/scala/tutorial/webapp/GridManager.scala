package tutorial.webapp

import org.scalajs.dom

class GridManager(
    painter: CanvasPainter,
    memRep: SpatialMemoryRepresentation,
    squareWidth: Int,
    metaData: MemoryMetaData
) {
  val gridRect = GridRectangle(0, 0, memRep.height, memRep.width)
  val boundaries = metaData.regions.map(r => computeBoundaries(r.range))
  val colors = Color.range(GridManager.RegionPaletteFrom, GridManager.RegionPaletteTo, boundaries.length)
  painter.canvas.width = memRep.width * squareWidth
  painter.canvas.height = memRep.height * squareWidth

  def processEventSeq(s: Seq[TraceEvent]): Unit = {
    s.zipWithIndex.foreach(processEvent)
    (boundaries zip colors).foreach(drawBoundaries(_, _))
    painter.refresh()
  }

  private def processEvent(e: TraceEvent, pos: Int): Unit = e match {
    case MemoryRead(address) =>
      drawWord(address, GridManager.ReadColor.copy(a = alphaAt(pos)))
    case MemoryWrite(address) =>
      drawWord(address, GridManager.WriteColor.copy(a = alphaAt(pos)))
  }

  private def alphaAt(pos: Int) = 1.0f / (pos+1)

  private def drawWord(address: Int, color: Color): Unit = {
    val square = memRep.addressToSquare(address)
    drawSquare(square, color)
  }

  private def drawSquare(s: GridSquare, color: Color): Unit = {
    painter.drawRect(squareWidth * s.col, squareWidth * s.row, squareWidth, squareWidth, color)
  }

  private def drawBoundaries(boundaries: Seq[(GridSquare, Orientation)], c: Color) =
    boundaries.foreach((s, o) => drawSquare(s, c))

  private def computeBoundaries(r: Range): Seq[(GridSquare, Orientation)] = {
    val rangeSquares = r.map(memRep.addressToSquare)
    for (
      s <- rangeSquares;
      (adj, or) <- s.allAdjacent if !gridRect.contains(adj) || !r.contains(memRep.squareToAddress(adj))
    ) yield (s, or)
  }
}

object GridManager {
  val ReadColor = Color(0, 0, 255, 1.0)
  val WriteColor = Color(0, 255, 0, 1.0)
  val RegionPaletteFrom = Color(249, 200, 14, 1.0)
  val RegionPaletteTo = Color(102, 46, 155, 1.0)
}
