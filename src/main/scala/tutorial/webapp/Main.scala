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

object TutorialApp {
  val squareWidth = 5.0
  val width = 200
  val height = 400
  val canvas = document.getElementById("canvas").asInstanceOf[html.Canvas]
  val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  val last = 50

  var paused = true
  var overrideT = -1
  

  def main(args: Array[String]): Unit = {
    canvas.width = (squareWidth * width).toInt
    canvas.height = (squareWidth * height).toInt
    
    val trace = BinaryTrace("./resources/binary.txt")

    trace.length().foreach { l =>
      
      val animation = Animation(refresh(trace), l)
      document.getElementById("range").setAttribute("max", l.toString)

      document
        .getElementById("range")
        .asInstanceOf[html.Input]
        .addEventListener("change", {e =>
          val value = e.target.asInstanceOf[html.Input].value.toInt
          animation.set(value)
        })
      
      document.getElementById("play").asInstanceOf[html.Button].addEventListener("click", {e =>
        animation.toggle()  
      })

      document.getElementById("next").asInstanceOf[html.Button].addEventListener("click", {e =>
        animation.move(1)  
      })

      document.getElementById("prev").asInstanceOf[html.Button].addEventListener("click", {e =>
        animation.move(-1)  
      })
    }

  }

  def refresh(trace: ProgramTrace)(i: Int): Future[Unit] = {
    trace.read(i, last).map { s =>
      document.getElementById("range").asInstanceOf[html.Input].value = i.toString
      processEventSeq(s)
    }
  }

  def processEventSeq(s: Seq[TraceEvent]): Unit =
    ctx.clearRect(0, 0, canvas.width, canvas.height)
    // println(s)
    s.zipWithIndex.foreach { (e, i) =>
      processEvent(e, i)
    }

  def processEvent(e: TraceEvent, i: Int): Unit = e match {
    case MemoryRead(address)  => drawWord(ctx, address, 1.0 / (i + 1))
    case MemoryWrite(address) => drawWord(ctx, address, 1.0 / (i + 1))
  }

  def drawWord(ctx: CanvasRenderingContext2D, i: Int, alpha: Double): Unit =
    drawSquare(ctx, i % width, i / width, alpha)

  def drawSquare(
      ctx: CanvasRenderingContext2D,
      i: Int,
      j: Int,
      alpha: Double
  ): Unit =
    ctx.clearRect(squareWidth * i, squareWidth * j, squareWidth, squareWidth)
    ctx.fillStyle = f"rgba(0, 0, 255, $alpha)"
    ctx.fillRect(squareWidth * i, squareWidth * j, squareWidth, squareWidth)
}
