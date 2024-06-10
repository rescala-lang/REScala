package tests.rescala.concurrency.philosophers

import reactives.core.ReInfo
import tests.rescala.concurrency.philosophers.PhilosopherTable.*

class DynamicPhilosopherTable[S](philosopherCount: Int)(ri: reactives.default.type)
    extends PhilosopherTable(philosopherCount)(ri) {
  import interface.{Signal, Var}

  override def createTable(tableSize: Int): Seq[Seating] = {
    def mod(n: Int): Int = (n + tableSize) % tableSize

    val phils = for i <- 0 until tableSize yield Var[Philosopher](Thinking)(using s"Phil($i)")

    val forks =
      for i <- 0 until tableSize yield {
        val nextCircularIndex      = mod(i + 1)
        implicit val name: ReInfo  = ReInfo.create.derive(s"Fork($i, $nextCircularIndex)")
        val left: Var[Philosopher] = phils(i)
        val right                  = phils(nextCircularIndex)
        Signal {
          left.value match {
            case Hungry => Taken(i.toString)
            case Thinking =>
              right.value match {
                case Hungry   => Taken(nextCircularIndex.toString)
                case Thinking => Free
              }
          }
        }
      }

    for i <- 0 until tableSize yield {
      val ownName               = i.toString
      val fork1                 = forks(i)
      val fork2                 = forks(mod(i - 1))
      implicit val info: ReInfo = ReInfo.create.derive(s"Vision($i)")
      val vision = Signal {
        fork1.value match {
          case Taken(`ownName`) => Eating
          case Taken(name)      => WaitingFor(name)
          case Free => fork2.value match {
              case Free        => Ready
              case Taken(name) => WaitingFor(name)
            }
        }
      }
      Seating(i, phils(i), fork1, fork2, vision)
    }
  }

}
