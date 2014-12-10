package philosophers

import java.util.concurrent.{LinkedBlockingQueue, ThreadPoolExecutor, TimeUnit}

import rescala.Signals.lift
import rescala.graph.Pulsing
import rescala.turns.Engines.pessimistic
import rescala.{DependentUpdate, Observe, Signal, Var}
import rescala.graph.DynamicsSupport.named

import scala.concurrent.{ExecutionContext, Future}



object REScalaPhilosophers extends App {
  val size = 3

  implicit val pool: ExecutionContext = ExecutionContext.fromExecutor(new ThreadPoolExecutor(
    0, 1, 1L, TimeUnit.SECONDS, new LinkedBlockingQueue[Runnable]))


  // ============================================= Infrastructure ========================================================

  sealed trait Philosopher
  case object Thinking extends Philosopher
  case object Eating extends Philosopher

  sealed trait Fork
  case object Free extends Fork
  case object Occupied extends Fork
  case object DoubleUsageError extends Fork

  val calcFork = { (leftState: Philosopher, rightState: Philosopher) =>
    if (leftState == Eating && rightState == Eating) {
      DoubleUsageError
    } else if (leftState == Eating || rightState == Eating) {
      Occupied
    } else {
      Free
    }
  }
  val calcReady = { (leftState: Fork, rightState: Fork) =>
    leftState == Free && rightState == Free
  }

  // ============================================ Entity Creation =========================================================

  case class Seating(placeNumber: Integer, philosopher: Var[Philosopher], leftFork: Signal[Fork], rightFork: Signal[Fork], canEat: Signal[Boolean])
  def createTable(tableSize: Int): Seq[Seating] = {
    val phils = for (i <- 0 until tableSize) yield {
      named(s"Phil$i")(Var[Philosopher](Thinking))
    }
    val forks = for (i <- 0 until tableSize) yield {
      named(s"Fork$i")(lift(phils(i), phils((i + 1) % tableSize))(calcFork))
    }
    val canEat = for (i <- 0 until tableSize) yield {
      named(s"canEat$i")(lift(forks(i), forks((i - 1 + tableSize) % tableSize))(calcReady))
    }
    for (i <- 0 until tableSize) yield {
      Seating(i, phils(i), forks(i), forks((i - 1 + tableSize) % tableSize), canEat(i))
    }
  }

  val seatings = createTable(size)
  val phils = seatings.map { _.philosopher }

  // ============================================== Logging =======================================================

  def log(msg: String): Unit = {
    println("[" + Thread.currentThread().getName + " @ " + System.currentTimeMillis() + "] " + msg)
  }
  def log[A](reactive: Pulsing[A]): Unit = {
    Observe(reactive) { value =>
      log(reactive + " now " + value)
    }
  }

  seatings.foreach { seating =>
    named(s"observePhil(${seating.placeNumber})")(log(seating.philosopher))
    named(s"observeFork(${seating.placeNumber})")(log(seating.leftFork))
    // right fork is the next guy's left fork
  }

  // ============================================ Runtime Behavior  =========================================================

  phils.zipWithIndex foreach { case (philosopher, i) =>
    named(s"think$i")(philosopher.observe { state =>
      if (state == Eating) {
        Future {
          philosopher set Thinking
        }
      }
    })
  }

  @annotation.tailrec // unrolled into loop by compiler
  def repeatUntilTrue(op: => Boolean): Unit = if (!op) repeatUntilTrue(op)

  def eatOnce(seating: Seating) = {
    repeatUntilTrue {
      DependentUpdate(seating.canEat) {
        (writes, canEat) =>
          if (canEat) {
            writes += seating.philosopher -> Eating
            true // Don't try again
          } else {
            false // Try again
          }
      }
    }
  }

  // ============================================== Thread management =======================================================

  object Spawn {
    def apply(name: String)(f: => Unit): Thread = {
      val t = new Thread(new Runnable {
        override def run(): Unit = f
      }, name)
      t.start()
      t
    }
  }

  // ===================== STARTUP =====================
  // start simulation
  @volatile private var killed = false
  log("Starting simulation. Press <Enter> to terminate!")
  val threads = seatings.map { seating =>
    val phil = seating.philosopher
    phil ->
      Spawn(seating.placeNumber.toString) {
        log("Controlling hunger on " + seating)
        while (!killed) {
          eatOnce(seating)
        }
        log(phil + " dies.")
      }
  }

  // ===================== SHUTDOWN =====================
  // wait for keyboard input
  Thread.sleep(3000)
  System.in.read()

  // kill all philosophers
  log("Received Termination Signal, Terminating...")
  killed = true

  // collect forked threads to check termination
  threads.foreach { case (phil, thread) =>
    import scala.language.postfixOps
    thread.join(50)
    if (!thread.isAlive) log(phil + " terminated.")
    else log(phil + " failed to terminate!")
  }
}
