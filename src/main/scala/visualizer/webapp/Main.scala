package visualizer.webapp

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

object VisualizerApp {
  val squareWidth = 3
  val width = 0x100
  val memSize = 0x10000
  val last = 10
  val canvas = document.getElementById("canvas").asInstanceOf[html.Canvas]
  val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

  def main(args: Array[String]): Unit = {    
    val trace = ProgramTrace("./resources/trace.bin", "./resources/index.bin", "./resources/phases.bin")

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
      buttonListener("faster", e => animation.accelerate())
      buttonListener("slower", e => animation.decelerate())
      buttonListener("reverse", e => animation.reverse())

      trace.phases().foreach(displayPhases(animation, _))
    }

  }

  def displayPhases(animation: Animation, phases: Iterator[(TracePhase, Int, Int)]) = {
    phases.toSeq.sortBy((_, s, _) => s) foreach { 
      case (phase, start, end) => {
        val elt = document.getElementById("phases")
        
        val child = dom.document.createElement("li")
        child.classList.add("list-group-item")

        
        val startButton = dom.document.createElement("button")
        startButton.setAttribute("type", "button")
        startButton.textContent = start.toString
        startButton.setAttribute("class", "btn btn-primary btn-sm btn-jump")
        
        startButton.asInstanceOf[html.Button].addEventListener("click", _ => animation.set(start))
        
        val endButton = dom.document.createElement("button")
        endButton.setAttribute("type", "button")
        endButton.textContent = end.toString
        endButton.setAttribute("class", "btn btn-primary btn-sm btn-jump")
        
        endButton.asInstanceOf[html.Button].addEventListener("click", _ => animation.set(end))
        
        child.append(startButton, " ", endButton, " ", phase.toString)
        elt.append(child)
      }
    }
  }

  def buttonListener(id: String, action: Event => Unit) =
    document.getElementById(id).asInstanceOf[html.Button].addEventListener("click", action)

  def refresh(trace: ProgramTrace, grid: GridManager)(i: Long, step: Int): Future[Unit] = {
    val nLast = if step == 0 then 1 else step
    trace.readBulk(i, nLast).map { s =>
      document.getElementById("range").asInstanceOf[html.Input].value = i.toString
      //grid.processEventSeq(s)
      grid.processBulk(s)

      if nLast > 1 then
        document.getElementById("info-time").textContent = f"${math.max(0, i - nLast + 1)} - $i"
      else
        document.getElementById("info-time").textContent = i.toString
      // document.getElementById("info-step").textContent = nLast.toString
    }
  }
}
