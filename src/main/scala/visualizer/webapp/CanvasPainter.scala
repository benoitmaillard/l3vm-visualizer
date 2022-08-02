package visualizer.webapp

import org.scalajs.dom

abstract class CanvasPainter(val canvas: dom.html.Canvas) {
  def resize(width: Int, height: Int): Unit = {
    canvas.width = width
    canvas.height = height
  }
  def drawRect(x: Int, y: Int, w: Int, h: Int, color: Color, layer: Int): Unit
  def refresh(layer: Int): Unit
}

// Draw rectangles using high-level API
class ShapePainter(canvas: dom.html.Canvas) extends CanvasPainter(canvas) {
  val ctx = canvas.getContext("2d", scalajs.js.Dictionary("alpha" -> false)).asInstanceOf[dom.CanvasRenderingContext2D]
  var clearRequired = false

  override def drawRect(x: Int, y: Int, w: Int, h: Int, c: Color, layer: Int): Unit = {
    //println(clearRequired)
    if (clearRequired) {
      ctx.clearRect(0, 0, canvas.width, canvas.height)
      clearRequired = false
    }
    
    ctx.clearRect(x, y, w, h)
    ctx.fillStyle = f"rgba(${c.r}, ${c.g}, ${c.b}, ${c.a})"
    ctx.fillRect(x, y, w, h)
  }
    
  override def refresh(layer: Int): Unit = {
    clearRequired = true
  }
}

// Draw rectangles pixel by pixel using the low-level ImageData API
class ArrayPainter(canvas: dom.html.Canvas, nLayers: Int) extends CanvasPainter(canvas) {
  val layers: Array[dom.html.Canvas] = (canvas +: (1 until nLayers).map(_ => dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas])).toArray
  val contexts = layers.map(c => c.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D])
  var buffers = contexts.map(_.createImageData(canvas.width, canvas.height))

  var paintCount = 0
  
  override def resize(width: Int, height: Int): Unit = {
    layers.foreach(c => {
      c.width = width
      c.height = height
    })
    buffers = contexts.map(_.createImageData(canvas.width, canvas.height))
  }

  override def drawRect(x: Int, y: Int, w: Int, h: Int, c: Color, layer: Int): Unit = {
    val start = System.currentTimeMillis()
    val buffer = buffers(layer)
    
    for (i <- 0 until h) {
      for (j <- 0 until w) {
        val (r, g, b, a) = c.toIntChannels
        buffer.data((i+y) * canvas.width * 4 + (j+x) * 4) = r
        buffer.data((i+y) * canvas.width * 4 + (j+x) * 4 + 1) = g
        buffer.data((i+y) * canvas.width * 4 + (j+x) * 4 + 2) = b
        buffer.data((i+y) * canvas.width * 4 + (j+x) * 4 + 3) = a
      }
    }
    paintCount += (System.currentTimeMillis() - start).toInt
  }

  override def refresh(layer: Int): Unit = {
    contexts(layer).putImageData(buffers(layer), 0, 0)
    buffers(layer) = contexts(layer).createImageData(canvas.width, canvas.height)

    var i = 0
    while (i < layers.length - 1) {
      contexts(i).drawImage(layers(i + 1), 0, 0)
      i += 1
    }

    paintCount = 0
  }
}