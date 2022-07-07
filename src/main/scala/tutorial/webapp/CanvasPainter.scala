package tutorial.webapp

import org.scalajs.dom

abstract class CanvasPainter(val canvas: dom.html.Canvas) {
  def drawRect(x: Int, y: Int, w: Int, h: Int, color: Color): Unit
  def refresh(): Unit
}

// Draw rectangles using high-level API
class ShapePainter(canvas: dom.html.Canvas) extends CanvasPainter(canvas) {
  val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  var clearRequired = false

  override def drawRect(x: Int, y: Int, w: Int, h: Int, c: Color): Unit = {
    //println(clearRequired)
    if (clearRequired) {
      ctx.clearRect(0, 0, canvas.width, canvas.height)
      clearRequired = false
    }
    
    ctx.clearRect(x, y, w, h)
    ctx.fillStyle = f"rgba(${c.r}, ${c.g}, ${c.b}, ${c.a})"
    ctx.fillRect(x, y, w, h)
  }
    
  override def refresh(): Unit = {
    clearRequired = true
  }
}

// Draw rectangles pixel by pixel using the low-level ImageData API
class ArrayPainter(canvas: dom.html.Canvas) extends CanvasPainter(canvas) {
  val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  var imagedata = ctx.createImageData(canvas.width, canvas.height);

  override def drawRect(x: Int, y: Int, w: Int, h: Int, c: Color): Unit = {  
    for (i <- 0 until h) {
      for (j <- 0 until w) {
        imagedata.data((i+y) * canvas.width * 4 + (j+x) * 4) = c.r
        imagedata.data((i+y) * canvas.width * 4 + (j+x) * 4 + 1) = c.g
        imagedata.data((i+y) * canvas.width * 4 + (j+x) * 4 + 2) = c.b
        imagedata.data((i+y) * canvas.width * 4 + (j+x) * 4 + 3) = (c.a * 255).toInt
      }
    }
  }

  override def refresh(): Unit = {
    ctx.putImageData(imagedata, 0, 0)
    imagedata = ctx.createImageData(canvas.width, canvas.height)
  }
}