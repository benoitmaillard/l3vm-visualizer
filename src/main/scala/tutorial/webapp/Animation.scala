package tutorial.webapp

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Promise
import scala.scalajs.js

class Animation(action: Long => Future[Unit], max: Long) {
  private var running = false
  private var increment = Animation.DefaultIncrement
  private var state = 0L

  def toggle(): Unit =
    running = !running
    if running then next()

  def move(diff: Long): Unit = set(mappedState + diff)

  def reset(): Unit = {
    increment = Animation.DefaultIncrement
    running = false
    setInternal(0)
  }

  def set(at: Long): Unit =
    setInternal(at * Animation.ResultInterval)

  def decelerate() = setIncrement(increment / 2)

  def accelerate() = setIncrement(increment * 2)

  def reverse() =
    increment = - increment

  def isRunning(): Boolean = running

  private def mappedState = state / Animation.ResultInterval

  private def setInternal(at: Long): Unit = {
    state = at
    if !running then action(mappedState)
  }

  private def setIncrement(newInc: Int): Unit =
    if newInc > Animation.MinIncrement && newInc < Animation.MaxIncrement then
      increment = newInc

  private def next(): Future[Unit] =
    val t = System.currentTimeMillis()
    if running then
      action(mappedState).flatMap { _ =>
        state += increment
        
        if (mappedState < 0 || mappedState >= max) {
          reset()
        }

        val elapsed = System.currentTimeMillis() - t
        val remaining = Animation.FrameInterval - elapsed
        if remaining > 0 then delay(remaining).flatMap(_ => next())
        else next()
      }
    else Future {}

  private def delay(milliseconds: Long): Future[Unit] = {
    val p = Promise[Unit]()
    js.timers.setTimeout(milliseconds) {
      p.success(())
    }
    p.future
  }
}

object Animation {
  val DefaultIncrement = 0x10
  val MinIncrement = 1
  val MaxIncrement = 0x1000000
  val ResultInterval = 64
  val FrameInterval = 16L // time in milliseconds between each iteration
}