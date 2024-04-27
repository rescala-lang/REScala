package benchmarks.philosophers

import benchmarks.philosophers.PhilosopherTable.*
import org.openjdk.jmh.infra.Blackhole
import reactives.operator.Interface

import java.util.concurrent.atomic.AtomicInteger

class PhilosopherTable(philosopherCount: Int, work: Long)(val engine: Interface) {

  import engine.*

  val seatings = createTable(philosopherCount)

  val eaten = new AtomicInteger(0)

  seatings.foreach { seating =>
    seating.vision.observe { state =>
      if (state == Done) {
        Blackhole.consumeCPU(work)
      }
    }
  }

  def createTable(tableSize: Int): Seq[Seating] = {
    def mod(n: Int): Int = (n + tableSize) % tableSize

    val phils = for (_ <- 0 until tableSize) yield Var[Philosopher](Thinking)

    val forks = for (i <- 0 until tableSize) yield {
      val nextCircularIndex = mod(i + 1)
      Signal.lift(phils(i), phils(nextCircularIndex))(calcFork(i.toString, nextCircularIndex.toString))
    }

    for (i <- 0 until tableSize) yield {
      val vision = Signal.lift(forks(i), forks(mod(i - 1)))(calcVision(i.toString))
      Seating(i, phils(i), forks(i), forks(mod(i - 1)), vision)
    }
  }

  def tryEat(seating: Seating): Boolean =
    engine.transactionWithWrapup(seating.philosopher) { t =>
      val forksAreFree = t.now(seating.vision) == Ready
      if (forksAreFree) seating.philosopher.admit(Eating)(t)
      forksAreFree
    } /* propagation executes here */ { (forksWereFree, t) =>
      if (forksWereFree) assert(t.now(seating.vision) == Done, s"philosopher should be done after turn")
      forksWereFree
    }

  // ============================================ Entity Creation =========================================================

  case class Seating(
      placeNumber: Int,
      philosopher: Var[Philosopher],
      leftFork: Signal[Fork],
      rightFork: Signal[Fork],
      vision: Signal[Vision]
  )
}

object PhilosopherTable {

  def calcFork(leftName: String, rightName: String)(leftState: Philosopher, rightState: Philosopher): Fork =
    (leftState, rightState) match {
      case (Thinking, Thinking) => Free
      case (Eating, _)          => Taken(leftName)
      case (_, Eating)          => Taken(rightName)
    }

  def calcVision(ownName: String)(leftFork: Fork, rightFork: Fork): Vision =
    (leftFork, rightFork) match {
      case (Free, Free)                         => Ready
      case (Taken(`ownName`), Taken(`ownName`)) => Done
      case (Taken(name), _)                     => BlockedBy(name)
      case (_, Taken(name))                     => BlockedBy(name)
    }

  // ============================================= Infrastructure ========================================================

  sealed trait Philosopher
  case object Thinking extends Philosopher
  case object Eating   extends Philosopher

  sealed trait Fork
  case object Free               extends Fork
  case class Taken(name: String) extends Fork

  sealed trait Vision
  case object Ready                  extends Vision
  case object Done                   extends Vision
  case class BlockedBy(name: String) extends Vision

}
