package tutorial.webapp

import org.scalajs.dom

class GridManager(
    canvas: dom.html.Canvas,
    memRep: SpatialMemoryRepresentation,
    squareWidth: Int
) {
  val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

  def setup() = {
    // TODO include space for margins (for drawing region boundaries)
    canvas.width = memRep.width * squareWidth
    canvas.height = memRep.height * squareWidth
  }

  def processEventSeq(s: Seq[TraceEvent]): Unit = {
    clear()
    s.zipWithIndex.foreach(processEvent)
  }

  private def processEvent(e: TraceEvent, pos: Int): Unit = e match {
    case MemoryRead(address) =>
      drawWord(address, GridManager.ReadColor.copy(a = alphaAt(pos)))
    case MemoryWrite(address) =>
      drawWord(address, GridManager.WriteColor.copy(a = alphaAt(pos)))
  }

  private def alphaAt(pos: Int) = 1.0f / (pos+1)

  private def clear(): Unit = ctx.clearRect(0, 0, canvas.width, canvas.height)

  private def drawWord(address: Int, color: GridManager.Color): Unit = {
    val (row, col) = memRep.addressToSquare(address)
    drawSquare(row, col, color)
  }

  private def drawSquare(row: Int, col: Int, color: GridManager.Color): Unit = {
    ctx.fillStyle = f"rgba(${color.r}, ${color.g}, ${color.b}, ${color.a})"
    ctx.fillRect(squareWidth * col, squareWidth * row, squareWidth, squareWidth)
  }
}

object GridManager {
  case class Color(r: Int, g: Int, b: Int, a: Float)
  val ReadColor = Color(0, 0, 255, 1.0)
  val WriteColor = Color(0, 255, 0, 1.0)
}
