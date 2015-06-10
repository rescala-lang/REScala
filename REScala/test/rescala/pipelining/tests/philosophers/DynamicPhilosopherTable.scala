package rescala.pipelining.tests.philosophers

import java.util.concurrent.atomic.AtomicInteger

import rescala.Signals._
import rescala.graph.Committable
import rescala.turns.{ Engine, Turn }
import rescala.{ Signal, Var }

import scala.annotation.tailrec

class DynamicPhilosopherTable(philosopherCount: Int, work: Long)(implicit val engine: Engine[Turn]) {

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

  def calcFork(leftName: String, rightName: String)(leftState: Signal[Philosopher], rightState: Signal[Philosopher])(turn : Turn): Fork = {
    val state = leftState(turn) match {
      case Thinking => rightState(turn) match {
        case Thinking => Free
        case Hungry => Taken(rightName)
      }
      case Hungry => Taken(leftName)
    }
    println(s"${Thread.currentThread().getId}: Fork between $leftName and $rightName is $state")
    state
  }

  def calcVision(ownName: String)(leftFork: Signal[Fork], rightFork: Signal[Fork])(implicit turn : Turn): Vision = {
    val vision = leftFork(turn) match {
      case Free => rightFork(turn) match {
        case Free => Ready
        case Taken(name) => WaitingFor(name)
      }
      case Taken(`ownName`) => rightFork(turn) match {
        case Taken(`ownName`) => Eating
        case Taken(name) => WaitingFor(name)
        case Free => WaitingFor(`ownName`)
      }
      case Taken(name) => WaitingFor(name)
    }
    println(s"${Thread.currentThread().getId}: $ownName has vision $vision")
    vision
  }

  def createTable(tableSize: Int): Seq[Seating] = {
    def mod(n: Int): Int = (n + tableSize) % tableSize

    val phils = for (i <- 0 until tableSize) yield Var[Philosopher](Thinking)

    val forks = for (i <- 0 until tableSize) yield {
      val nextCircularIndex = mod(i + 1)
    //  lift(phils(i), phils(nextCircularIndex))(calcFork(i.toString, nextCircularIndex.toString))
      dynamic()(turn => calcFork(i.toString, nextCircularIndex.toString)(phils(i), phils(nextCircularIndex))(turn))
      
    }

    for (i <- 0 until tableSize) yield {
      val leftFork = forks(i)
      val rightFork = forks(mod(i - 1))
      val vision = dynamic() (turn => calcVision(i.toString)(leftFork, rightFork)(turn))
      Seating(i, phils(i), leftFork, rightFork, vision)
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

