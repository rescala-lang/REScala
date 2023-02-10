package tests.rescala.concurrency.philosophers

import rescala.core.{ReInfo}
import rescala.interface.RescalaInterface
import tests.rescala.concurrency.philosophers.PhilosopherTable._

class DynamicPhilosopherTable[S](philosopherCount: Int)(ri: RescalaInterface)
    extends PhilosopherTable(philosopherCount)(ri) {
  import interface.{Var, Signal, implicitScheduler}

  override def createTable(tableSize: Int): Seq[Seating] = {
    def mod(n: Int): Int = (n + tableSize) % tableSize

    val phils = for (i <- 0 until tableSize) yield Var[Philosopher](Thinking)(s"Phil($i)")

    val forks = for (i <- 0 until tableSize) yield {
      val nextCircularIndex      = mod(i + 1)
      implicit val name: ReInfo  = ReInfo.create.derive(s"Fork($i, $nextCircularIndex)")
      val left: Var[Philosopher] = phils(i)
      val right                  = phils(nextCircularIndex)
      Signal {
        left.value match {
          case Hungry => Taken(i.toString)
          case Thinking =>
            right() match {
              case Hungry   => Taken(nextCircularIndex.toString)
              case Thinking => Free
            }
        }
      }
    }

    for (i <- 0 until tableSize) yield {
      val ownName               = i.toString
      val fork1                 = forks(i)
      val fork2                 = forks(mod(i - 1))
      implicit val info: ReInfo = ReInfo.create.derive(s"Vision($i)")
      val vision = Signal {
        fork1() match {
          case Taken(`ownName`) => Eating
          case Taken(name)      => WaitingFor(name)
          case Free => fork2() match {
              case Free        => Ready
              case Taken(name) => WaitingFor(name)
            }
        }
      }
      Seating(i, phils(i), fork1, fork2, vision)
    }
  }

}
