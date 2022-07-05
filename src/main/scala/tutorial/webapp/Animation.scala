package tutorial.webapp

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Promise
import scala.scalajs.js

class Animation(action: Int => Future[Unit], max: Int) {
  private var running = false
  private var reversed = false
  private var interval = Animation.DefaultInterval
  private var state = 0

  private def next(): Future[Unit] =
    val t = System.currentTimeMillis()
    if running && state >= 0 && state < max then
      action(state).flatMap { _ =>
        state += (if reversed then -1 else 1)
        
        if (state < 0 || state >= max) {
          reset()
        }

        val elapsed = System.currentTimeMillis() - t
        val remaining = interval - elapsed
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

  def toggle(): Unit =
    running = !running
    if running then next()

  def move(diff: Int): Unit = set(state + diff)

  def reset(): Unit = {
    interval = Animation.DefaultInterval
    running = false
    reversed = false
    set(0)
  }

  def set(at: Int): Unit = {
    state = at
    if !running then action(state)
  }

  def setInterval(newInterval: Long): Unit = {
    interval = newInterval
  }

  def reverse(): Unit =
    reversed = !reversed

  def isRunning(): Boolean = running

  def getInterval(): Long = interval
}

object Animation {
  val DefaultInterval = 50L
}