package benchmarks.philosophers

import java.util.concurrent.atomic.AtomicInteger

import benchmarks.philosophers.PhilosopherTable._
import org.openjdk.jmh.infra.Blackhole
import rescala.Signals.lift
import rescala.graph.Spores
import rescala.turns.{Engine, Turn}
import rescala.{Signal, Var}

import scala.annotation.tailrec

class PhilosopherTable[S <: Spores](philosopherCount: Int, work: Long)(implicit val engine: Engine[S, Turn[S]]) {


  val seatings = createTable(philosopherCount)

  val eaten = new AtomicInteger(0)

  seatings.foreach { seating =>
    seating.vision.observe { state =>
      if (state == Eating) {
        Blackhole.consumeCPU(work)
      }
    }
  }


  def calcFork(leftName: String, rightName: String)(leftState: Philosopher, rightState: Philosopher): Fork =
    (leftState, rightState) match {
      case (Thinking, Thinking) => Free
      case (Hungry, _) => Taken(leftName)
      case (_, Hungry) => Taken(rightName)
    }

  def calcVision(ownName: String)(leftFork: Fork, rightFork: Fork): Vision =
    (leftFork, rightFork) match {
      case (Free, Free) => Ready
      case (Taken(`ownName`), Taken(`ownName`)) => Eating
      case (Taken(name), _) => WaitingFor(name)
      case (_, Taken(name)) => WaitingFor(name)
    }


  def createTable(tableSize: Int): Seq[Seating[S]] = {
    def mod(n: Int): Int = (n + tableSize) % tableSize

    val phils = for (i <- 0 until tableSize) yield Var[Philosopher, S](Thinking)

    val forks = for (i <- 0 until tableSize) yield {
      val nextCircularIndex = mod(i + 1)
      lift(phils(i), phils(nextCircularIndex))(calcFork(i.toString, nextCircularIndex.toString))
    }

    for (i <- 0 until tableSize) yield {
      val vision = lift(forks(i), forks(mod(i - 1)))(calcVision(i.toString))
      Seating(i, phils(i), forks(i), forks(mod(i - 1)), vision)
    }
  }


  def tryEat(seating: Seating[S]): Boolean =
    implicitly[Engine[S, Turn[S]]].plan(seating.philosopher) { turn =>
      val forksWereFree = if (seating.vision(turn) == Ready) {
        seating.philosopher.admit(Hungry)(turn)
        true
      }
      else false
      turn.observe { if (forksWereFree) assert(seating.vision(turn) == Eating) }
      forksWereFree
    }

  def eatOnce(seating: Seating[S]) = repeatUntilTrue(tryEat(seating))

}

object PhilosopherTable {


  // ============================================= Infrastructure ========================================================

  sealed trait Philosopher
  case object Thinking extends Philosopher
  case object Hungry extends Philosopher

  sealed trait Fork
  case object Free extends Fork
  case class Taken(name: String) extends Fork

  sealed trait Vision
  case object Ready extends Vision
  case object Eating extends Vision
  case class WaitingFor(name: String) extends Vision


  // ============================================ Entity Creation =========================================================

  case class Seating[S <: Spores](placeNumber: Int, philosopher: Var[Philosopher, S], leftFork: Signal[Fork, S], rightFork: Signal[Fork, S], vision: Signal[Vision, S])



  @tailrec // unrolled into loop by compiler
  final def repeatUntilTrue(op: => Boolean): Unit = if (!op) repeatUntilTrue(op)


}
