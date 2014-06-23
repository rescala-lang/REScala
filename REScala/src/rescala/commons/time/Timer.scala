package rescala.commons.time

import rescala.events._
import rescala.SignalSynt
import rescala.Var
import rescala.Signal
import scala.collection.SortedSet

/**
 * A Timer class for FRP purposes.
 * After creation, all timers need to be run with Timer.runAll
 */
class Timer(val interval: Time) extends Ordered[Timer] {
  type Seconds = Double

  // Timer is a mutable type, but it is ordered by a static property
  def compare(that: Timer) = (this.interval - that.interval).s.toInt

  /** Tick event gets triggered every 'interval' milliseconds.
   *  Passes the real delta as parameter due to thread sleeping inaccuracy */
  val tick = new ForkedEvent[Seconds]

  /** Tick event that drops the delta for convenience */
  val tock : Event[Unit] = tick.dropParam

  /** Signal for the total time */
  val time = tick.fold(0.0) {(total: Seconds, delta: Seconds) => total + delta}

  /** Returns the integral of the Signal s over time */
  def integral(s: Signal[Seconds]) : Signal[Seconds] = {
    // simple Riemann integral, could do more
    return tick.fold(0.0) {(total : Seconds, delta : Seconds) =>
      total + delta * s()
    }
  }

  /** Integrates a Signal expression over time */
  def integrate(expr : => Seconds) : Signal[Seconds] = {
    return tick.fold(0.0) {(total : Seconds, delta : Seconds) =>
      total + delta * expr
    }
  }

  /** Returns a new Signal that counts the local time from now */
  def localTime : Signal[Seconds] = {
    val now = time()
    return SignalSynt {s: SignalSynt[Seconds] => time() - now}
  }

  /** Returns a Signal which is true if the specified delay has passed */
  def passed(delay : Seconds) : Signal[Boolean] = {
    val now = time()
    return SignalSynt {s: SignalSynt[Boolean] => time(s) > now + delay }
  }

  /** Returns a new event which fires exactly once after the specified delay */
  def after(delay : Seconds) : Event[Unit] = passed(delay).changedTo(true)

  /** Snapshots a signal for a given time window */
  def timeWindow[A](window : Seconds)(s : Signal[A]) : Signal[Seq[A]] = {
    if(interval == 0) throw new RuntimeException("You must use an interval > 0")
    val delta = interval.s
    val n = (window / delta).asInstanceOf[Int]
    (tick snapshot s).changed.last(n)
  }
  
  /** Samples this expression on each occurence of the timer */
  def sample[T](expr: => T): Signal[T] = {
    this.tock.set(None)(_ => expr)
  }

  // globally register this timer
  Timer.register(this)
}


object Timer {

  /** Factory method to create timers */
  def apply(interval: Time): Timer = new Timer(interval)

  // a global ticker which triggers as often as needed
  val globaltick = new ImperativeEvent[Time]

  case class Scheduled(remaining: Long, timer: Timer)
  extends Ordered[Scheduled] {
	def compare(that: Scheduled) = (this.remaining - that.remaining).toInt
	def postpone(offset : Int) = Scheduled(remaining - offset, timer)
	def reschedule = Scheduled(timer.interval.ms, timer)
  }

  protected var schedule: List[Scheduled] = List()
  protected def register(t : Timer) = schedule :+= Scheduled(t.interval.ms, t)
  protected def unregister(t : Timer) = schedule = schedule.filterNot(_.timer == t)


  protected var lastTick: Long = 0

  /**
   * Runs all created Timer objects in this thread (blocking).
   */
  def runAll() {
    while(!schedule.isEmpty)
      tickNext
  }

  def tickNext() {
    val delta = schedule.min.remaining
    val (tickNow, tickLater) = schedule.partition {_.remaining == delta}

    // sleep to approximate current delta, assume a min. passed time of 1 ms
    val now = System.nanoTime()
    val tNeeded = (now - (if(lastTick == 0) now else lastTick)) / 1000 + 1
    val tSleep = math.max(1, delta - tNeeded)

    Thread.sleep(tSleep)

    // measure real delta
    val realDelta = math.max(tNeeded, delta).asInstanceOf[Int]
    val realSecs = realDelta / 1000.0

    // tick timer subset
    val tickset = tickNow.map(_.timer.tick)
    val ticker = new ImperativeForkEvent[Double](tickset: _*)
    ticker(realSecs)

    // update the schedule
    val rescheduled = tickNow.map (_.reschedule)
    val postponed = tickLater.map (_ postpone realDelta)
    schedule = rescheduled ++ postponed

    // record the time
    lastTick = System.nanoTime()
  }
}
