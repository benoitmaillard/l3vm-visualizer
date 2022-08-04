package visualizer.webapp

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Promise
import scala.scalajs.js

/** Class for executing an action at regular intervals. The animation works
 *  by maintaining an internal state and incrementing this state at each tick.
 *  This internal state is multiplied by some factor in order to produce the
 *  external state. The external state is the time exposed to the user. This
 *  mechanism allows the external state to be incremented only once every
 *  n internal step (n being the aforementioned factor) if desired.
 * 
 * @constructor create a new animation
 * @param action function to execute at each update. The current time and
 *  the length of the step (difference between previous execution and current one)
 *  are passed as arguments. The action is also executed at instanciation
 * @param max time at which the animation should reset
 */
class Animation(action: (Long, Int) => Future[Unit], max: Long) {
  private var running = false
  private var increment = Animation.InitialIncrement
  private var state = 0L

  // Display initial state
  action(mappedState, 0)

  /** Toggles the state of the animation */
  def toggle(): Unit =
    running = !running
    if running then next()

  /** Moves the animation forward based on the provided step
   * 
   * @param diff number of steps
  */
  def move(diff: Long): Unit = set(mappedState + diff)

  /** Sets the animation back to initial state */
  def reset(): Unit = {
    increment = Animation.InitialIncrement
    running = false
    setInternal(0)
  }

  /** Moves the animation to the provided state
   * 
   * @param at new state
  */
  def set(at: Long): Unit =
    setInternal(at * Animation.ResultInterval)

  /** Halves the speed of the animation */
  def decelerate() = setIncrement(increment / 2)

  /** Doubles the speed of the animation */
  def accelerate() = setIncrement(increment * 2)

  /** Reverse the animation */
  def reverse() =
    increment = - increment

  /** Indicates if the animation is running */
  def isRunning(): Boolean = running

  private def mappedState = state / Animation.ResultInterval

  private def setInternal(at: Long): Unit = {
    state = at
    if (mappedState < 0 || mappedState >= max) {
      reset()
    }
    if !running then action(mappedState, increment / Animation.ResultInterval)
  }

  private def setIncrement(newInc: Int): Unit =
    if newInc > Animation.MinIncrement && newInc < Animation.MaxIncrement then
      increment = newInc

  private def next(): Future[Unit] =
    val t = System.currentTimeMillis()
    action(mappedState, increment / Animation.ResultInterval).flatMap { _ =>
      if running then
        setInternal(state + increment)
        
        val elapsed = System.currentTimeMillis() - t
        val remaining = Animation.FrameInterval - elapsed
        if remaining > 0 then delay(remaining).flatMap(_ => next())
        else delay(1).flatMap(_ => next())
      else Future {}
    }

  private def delay(milliseconds: Long): Future[Unit] = {
    val p = Promise[Unit]()
    js.timers.setTimeout(milliseconds) {
      p.success(())
    }
    p.future
  }
}

object Animation {
  val InitialIncrement = 0x10
  val MinIncrement = 1
  val MaxIncrement = 0x1000000
  
  // Factor by which the internal state is multiplied to provide the exposed state
  val ResultInterval = 64
  
  // Time in milliseconds between each internal tick
  val FrameInterval = 16L
}