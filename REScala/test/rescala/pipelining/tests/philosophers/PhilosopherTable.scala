package rescala.pipelining.tests.philosophers

import java.util.concurrent.atomic.AtomicInteger

import rescala.Signals.lift
import rescala.graph.Committable
import rescala.turns.{ Engine, Turn }
import rescala.{ Signal, Var }

import scala.annotation.tailrec

class PhilosopherTable(philosopherCount: Int, work: Long)(implicit val engine: Engine[Turn]) {

  import rescala.pipelining.tests.philosophers.PhilosopherTable._

  val seatings = createTable(philosopherCount)

  val eaten = new AtomicInteger(0)

  seatings.foreach { seating =>
    seating.vision.observe { state =>
      if (state == Eating) {
        eaten.incrementAndGet()
      }
    }
  }

  def calcFork(leftName: String, rightName: String)(leftState: Philosopher, rightState: Philosopher): Fork = {
    val state = (leftState, rightState) match {
      case (Thinking, Thinking) => Free
      case (Hungry, _)          => Taken(leftName)
      case (_, Hungry)          => Taken(rightName)
    }
    println(s"${Thread.currentThread().getId}: Fork between $leftName and $rightName is $state")
    state
  }

  def calcVision(ownName: String)(leftFork: Fork, rightFork: Fork): Vision = {
    val vision = (leftFork, rightFork) match {
      case (Free, Free)                         => Ready
      case (Taken(`ownName`), Taken(`ownName`)) => Eating
      case (Taken(name), _)                     => WaitingFor(name)
      case (_, Taken(name))                     => WaitingFor(name)
    }
    println(s"${Thread.currentThread().getId}: $ownName has vision $vision")
    vision
  }

  def createTable(tableSize: Int): Seq[Seating] = {
    def mod(n: Int): Int = (n + tableSize) % tableSize

    val phils = for (i <- 0 until tableSize) yield Var[Philosopher](Thinking)

    val forks = for (i <- 0 until tableSize) yield {
      val nextCircularIndex = mod(i + 1)
      lift(phils(i), phils(nextCircularIndex))(calcFork(i.toString, nextCircularIndex.toString))
    }

    for (i <- 0 until tableSize) yield {
      val vision = lift(forks(i), forks(mod(i - 1)))(calcVision(i.toString))
      Seating(i, phils(i), forks(i), forks(mod(i - 1)), vision)
    }
  }

  def tryEat(seating: Seating): Boolean =
    engine.plan(seating.philosopher) { turn =>
      val forksFree = if (seating.vision(turn) == Ready) {
        println(s"${Thread.currentThread().getId}: ${seating.placeNumber} is hungry")
        assert(seating.leftFork(turn) == Free)
        assert(seating.rightFork(turn) == Free)
        seating.philosopher.admit(Hungry)(turn)
        true
      } else {
        //  println(s"${Thread.currentThread().getId}: ${seating.placeNumber} is thinking")
        false
      }
      turn.schedule(new Committable {
        override def commit(implicit turn: Turn): Unit = if (forksFree) {
          assert(seating.vision(turn) == Eating, s"Wrong result for ${Thread.currentThread().getId}")
          assert(seating.leftFork(turn) == Taken(seating.placeNumber.toString()))
          assert(seating.leftFork(turn) == Taken(seating.placeNumber.toString()))
          assert(seating.philosopher(turn) == Hungry)
        }
        override def release(implicit turn: Turn): Unit = ()
      })
      forksFree
    }

  def eatOnce(seating: Seating) = repeatUntilTrue(tryEat(seating))

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

  case class Seating(placeNumber: Int, philosopher: Var[Philosopher], leftFork: Signal[Fork], rightFork: Signal[Fork], vision: Signal[Vision])

  @tailrec // unrolled into loop by compiler
  final def repeatUntilTrue(op: => Boolean): Unit = if (!op) repeatUntilTrue(op)

}
