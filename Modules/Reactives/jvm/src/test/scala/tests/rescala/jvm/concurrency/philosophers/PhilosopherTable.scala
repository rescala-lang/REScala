package tests.rescala.concurrency.philosophers

import reactives.core.Transaction
import reactives.parrp.Backoff

import java.util.concurrent.atomic.AtomicInteger

class PhilosopherTable(philosopherCount: Int)(val interface: reactives.default.type ) {
  import interface.*
  import tests.rescala.concurrency.philosophers.PhilosopherTable.*

  val seatings: Seq[Seating] = createTable(philosopherCount)

  val eaten: AtomicInteger = new AtomicInteger(0)

  seatings.foreach { seating =>
    seating.vision.observe { state =>
      if state == Eating then {
        eaten.incrementAndGet()
        ()
      }
    }(using s"Observer ${seating.vision}")
  }

  def calcFork(leftName: String, rightName: String)(leftState: Philosopher, rightState: Philosopher): Fork =
    (leftState, rightState) match {
      case (Thinking, Thinking) => Free
      case (Hungry, _)          => Taken(leftName)
      case (_, Hungry)          => Taken(rightName)
    }

  def calcVision(ownName: String)(leftFork: Fork, rightFork: Fork): Vision =
    (leftFork, rightFork) match {
      case (Free, Free)                         => Ready
      case (Taken(`ownName`), Taken(`ownName`)) => Eating
      case (Taken(name), _)                     => WaitingFor(name)
      case (_, Taken(name))                     => WaitingFor(name)
    }

  def createTable(tableSize: Int): Seq[Seating] = {
    def mod(n: Int): Int = (n + tableSize) % tableSize

    val phils = for i <- 0 until tableSize yield Var[Philosopher](Thinking)(using s"Phil($i)")

    val forks =
      for i <- 0 until tableSize yield {
        val nextCircularIndex = mod(i + 1)
        interface.Signal.lift(phils(i), phils(nextCircularIndex))(calcFork(i.toString, nextCircularIndex.toString))(
          using s"Fork($i, $nextCircularIndex)"
        )
      }

    for i <- 0 until tableSize yield {
      val previousCircularIndex = mod(i - 1)
      val vision =
        interface.Signal.lift(forks(previousCircularIndex), forks(i))(calcVision(i.toString))(using s"Vision($i)")
      Seating(i, phils(i), forks(previousCircularIndex), forks(i), vision)
    }
  }

  def tryEat(seating: Seating): Boolean =
    interface.transactionWithWrapup(seating.philosopher) { turn =>
      val forksAreFree = turn.now(seating.vision) == Ready
      if forksAreFree then {
        // println(Thread.currentThread().getName + " start " + turn + ": Eating on " + seating.philosopher)
        seating.philosopher.admit(Hungry)(using turn)
      } // else {
      //   println(Thread.currentThread().getName + " start " + turn + ": No Change on " + seating.philosopher)
      // }
      forksAreFree
    } /* propagation executes here */ { (forksWereFree, turn) =>
      if forksWereFree then
        assert(
          turn.now(seating.vision) == Eating,
          s"philosopher should be done after turn but is ${seating.inspect(turn)}"
        )
      // println(Thread.currentThread().getName + " done " + turn)
      forksWereFree
    }

  def eatOnce(seating: Seating): Unit = {
    val bo = new Backoff()
    while !tryEat(seating) do { bo.backoff() }

    seating.philosopher.set(Thinking)
    // engine.transactionWithWrapup(seating.philosopher){ turn =>
    //   println(Thread.currentThread().getName + " start " + turn + ": Thinking on " + seating.philosopher)
    //   seating.philosopher.admit(Thinking)(turn)
    // } { (x, turn) =>
    //   println(Thread.currentThread().getName + " done " + turn)
    // }
  }

  // ============================================ Entity Creation =========================================================

  case class Seating(
      placeNumber: Int,
      philosopher: Var[Philosopher],
      leftFork: Signal[Fork],
      rightFork: Signal[Fork],
      vision: Signal[Vision]
  ) {
    def inspect(t: Transaction[reactives.SelectedScheduler.State]): String =
      s"Seating(${t.now(philosopher)}, ${t.now(leftFork)}, ${t.now(rightFork)}, ${t.now(vision)})"
  }

}

object PhilosopherTable {
  // ============================================= Infrastructure ========================================================

  sealed trait Philosopher
  case object Thinking extends Philosopher
  case object Hungry   extends Philosopher

  sealed trait Fork
  case object Free               extends Fork
  case class Taken(name: String) extends Fork

  sealed trait Vision
  case object Ready                   extends Vision
  case object Eating                  extends Vision
  case class WaitingFor(name: String) extends Vision

}
