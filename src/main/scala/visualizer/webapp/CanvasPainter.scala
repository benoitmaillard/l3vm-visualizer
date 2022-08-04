package visualizer.webapp

import org.scalajs.dom

/** Tool to draw rectangles on a canvas using the low-level ImageData API.
 *  The layer with at index 0 is the bottom layer. Drawings on a given
 *  layer will appear on top of drawings on layers with a lower index.
 *  For performance reaons, pixels of lower layers are overwritten and
 *  transparency does not play any role.
 * 
 * @constructor create a canvas painter
 * @param canvas canvas on which the drawing takes place
 * @param nLayers number of layers
 * 
*/
class CanvasPainter(canvas: dom.html.Canvas, nLayers: Int) {
  private val layers: Array[dom.html.Canvas] = (canvas +: (1 until nLayers).map(_ => dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas])).toArray
  private val contexts = layers.map(c => c.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D])
  private var buffers = contexts.map(_.createImageData(canvas.width, canvas.height))

  var paintCount = 0
  
  /** Changes the dimensions of the buffer.
   * 
   * @param width width of the new canvas in pixels
   * @param height height of the new canvas in pixels
  */
  def resize(width: Int, height: Int): Unit = {
    layers.foreach(c => {
      c.width = width
      c.height = height
    })
    buffers = contexts.map(_.createImageData(canvas.width, canvas.height))
  }

  /** Draws a rectangle on one of the layers
   * 
   * @param x top-left x coordinate
   * @param y top-left y coordinate
   * @param w width
   * @param h height
   * @param c color
   * @param layer layer on which the drawing takes place
  */
  def drawRect(x: Int, y: Int, w: Int, h: Int, c: Color, layer: Int): Unit = {
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

  /** Refreshes the image from the given layer until the top layer.
   *  Lower layers are not updated.
   * 
   * @param layer layer from which the canvas is updated
   * 
  */
  def refresh(layer: Int): Unit = {
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