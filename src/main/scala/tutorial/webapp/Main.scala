package tutorial.webapp

import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.html

import scala.scalajs.js
import js.JSConverters._

// canvas imports
import org.scalajs.dom.ImageData
import org.scalajs.dom.CanvasRenderingContext2D

import scala.concurrent.ExecutionContext.Implicits.global
import js.Thenable.Implicits._
import scala.util.Success
import scala.util.Failure
import org.scalajs.dom.RequestInit
import org.scalajs.dom.Request
import org.scalajs.dom.Headers
import scala.concurrent.Future
import scala.concurrent.Promise
import org.scalajs.dom.Event

object TutorialApp {
  val squareWidth = 5
  val width = 0x100
  val memSize = 0x10000
  val last = 3
  val canvas = document.getElementById("canvas").asInstanceOf[html.Canvas]
  val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

  def main(args: Array[String]): Unit = {    
    val trace = BinaryTrace("./resources/trace.bin")

    trace.length().foreach { l =>
      val memRep = ScanMemoryRepresentation(width, math.ceil(memSize.toDouble / width).toInt)
      val regions = Seq(
        MemoryRegion(0 until 572, "Code"),
        MemoryRegion(572 until 1090, "Top-frame"),
        MemoryRegion(1090 until 8633, "Bitmap"),
        MemoryRegion(8633 until 250000, "Heap"),
      )
      val metaData = MemoryMetaData(regions, memSize)
      // val painter = ShapePainter(canvas)
      val painter = ArrayPainter(canvas, 2)
      val grid = GridManager(painter, memRep, squareWidth, metaData)
      
      val animation = Animation(refresh(trace, grid), l)
      document.getElementById("range").setAttribute("max", l.toString)

      document
        .getElementById("range")
        .asInstanceOf[html.Input]
        .addEventListener("change", {e =>
          val value = e.target.asInstanceOf[html.Input].value.toInt
          animation.set(value)
        })
      
      buttonListener("play", e => animation.toggle())
      buttonListener("reset", e => animation.reset())
      buttonListener("prev", e => animation.move(-1))
      buttonListener("next", e => animation.move(1))
      buttonListener("faster", e => animation.setInterval(animation.getInterval() * 2))
      buttonListener("slower", e => animation.setInterval(animation.getInterval() / 2))
      buttonListener("reverse", e => animation.reverse())
    }

  }

  def buttonListener(id: String, action: Event => Unit) =
    document.getElementById(id).asInstanceOf[html.Button].addEventListener("click", action)

  def refresh(trace: ProgramTrace, grid: GridManager)(i: Int): Future[Unit] = {
    trace.read(i, last).map { s =>
      document.getElementById("range").asInstanceOf[html.Input].value = i.toString
      grid.processEventSeq(s)
    }
  }
}
