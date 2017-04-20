package tests.rescala.concurrency.philosophers

import java.util.concurrent.atomic.AtomicInteger

import rescala.engine.{Engine, Turn}
import rescala.util.Globals.named
import rescala.graph.Struct
import rescala.parrp.Backoff
import rescala.reactives.{Signal, Var}
import rescala.reactives.Signals.lift
import rescala.twoversion.{Committable, CommonPropagationImpl, TwoVersionPropagation}

class PhilosopherTable[S <: Struct](philosopherCount: Int, work: Long)(implicit val engine: Engine[S, Turn[S]]) {

  import engine.Var
  import tests.rescala.concurrency.philosophers.PhilosopherTable._

  val seatings = createTable(philosopherCount)

  val eaten = new AtomicInteger(0)

  seatings.foreach { seating =>
    named(s"Observer ${seating.vision}") {
      seating.vision.observe { state =>
        if (state == Eating) {
          eaten.incrementAndGet()
        }
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

    val phils = for (i <- 0 until tableSize) yield named(s"Phil($i)")(Var[Philosopher](Thinking))

    val forks = for (i <- 0 until tableSize) yield {
      val nextCircularIndex = mod(i + 1)
      named(s"Fork($i, $nextCircularIndex)")(lift(phils(i), phils(nextCircularIndex))(calcFork(i.toString, nextCircularIndex.toString)))
    }

    for (i <- 0 until tableSize) yield {
      val vision = named(s"Vision($i, ${mod(i - 1)})")(lift(forks(i), forks(mod(i - 1)))(calcVision(i.toString)))
      Seating(i, phils(i), forks(i), forks(mod(i - 1)), vision)
    }
  }


  def tryEat(seating: Seating[S]): Boolean =
    engine.transaction(seating.philosopher) { turn =>
      val forksWereFree = seating.vision.now(turn) == Ready
      if (forksWereFree) seating.philosopher.admit(Hungry)(turn)
      turn match {
        case cmn: CommonPropagationImpl[S] =>
          cmn.schedule(new Committable[S] {
            override def commit(implicit t: TwoVersionPropagation[S]): Unit = if (forksWereFree) assert(seating.vision.now(turn) == Eating, s"philosopher should be done after turn but is ${seating.inspect(turn)}")
            override def release(implicit t: TwoVersionPropagation[S]): Unit = () /*assert(assertion = false, "turn should not rollback")*/ // assertion is unnecessary, exception propagation will take care
          })
        case _ =>
      }

      forksWereFree
    }

  def eatOnce(seating: Seating[S]): Unit = {
    val bo = new Backoff()
    while(!tryEat(seating)) {bo.backoff()}
  }

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

  case class Seating[S <: Struct](placeNumber: Int, philosopher: Var[Philosopher, S], leftFork: Signal[Fork, S], rightFork: Signal[Fork, S], vision: Signal[Vision, S]) {
    def inspect(t: Turn[S]): String = s"Seating(${philosopher.now(t)}, ${leftFork.now(t)}, ${rightFork.now(t)}, ${vision.now(t)})"
  }

}
