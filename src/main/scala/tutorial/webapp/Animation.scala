package tutorial.webapp

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Promise
import scala.scalajs.js

class Animation(action: Int => Future[Unit], max: Int) {
  private var running = false
  private var interval: Long = 50
  private var state = 0

  private def next(): Future[Unit] =
    val t = System.currentTimeMillis()
    if running then
      action(state).flatMap { _ =>
        if state >= max then {
          running = false
        }

        state += 1

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

  def reset(): Unit = set(0)

  def set(at: Int): Unit = {
    state = at
    if !running then action(state)
  }

  def setInterval(newInterval: Long): Unit = {
    interval = newInterval
  }

  def isRunning(): Boolean = running
}
