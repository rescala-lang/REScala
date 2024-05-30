package benchmarks.philosophers

import benchmarks.philosophers.PhilosopherTable.*
import reactives.operator.Interface

class DynamicPhilosopherTable(philosopherCount: Int, work: Long)(override val engine: Interface)
    extends PhilosopherTable(philosopherCount, work)(engine) {

  import engine.*

  override def createTable(tableSize: Int): Seq[Seating] = {
    def mod(n: Int): Int = (n + tableSize) % tableSize

    val phils = for _ <- 0 until tableSize yield Var[Philosopher](Thinking)

    val forks = for i <- 0 until tableSize yield {
      val nextCircularIndex = mod(i + 1)
      Signal.dynamic {
        phils(i).value match {
          case Eating => Taken(i.toString)
          case Thinking =>
            phils(nextCircularIndex).value match {
              case Eating   => Taken(nextCircularIndex.toString)
              case Thinking => Free
            }
        }
      }

    }

    for i <- 0 until tableSize yield {
      val ownName = i.toString
      val vision = Signal.dynamic {
        forks(i).value match {
          case Taken(`ownName`) => Done
          case Taken(name)      => BlockedBy(name)
          case Free => forks(mod(i - 1)).value match {
              case Free        => Ready
              case Taken(name) => BlockedBy(name)
            }
        }
      }
      Seating(i, phils(i), forks(i), forks(mod(i - 1)), vision)
    }
  }

}

class HalfDynamicPhilosopherTable(philosopherCount: Int, work: Long)(
    override val engine: Interface
) extends PhilosopherTable(philosopherCount, work)(engine) {

  import engine.*

  override def createTable(tableSize: Int): Seq[Seating] = {
    def mod(n: Int): Int = (n + tableSize) % tableSize

    val phils = for _ <- 0 until tableSize yield Var[Philosopher](Thinking)

    val forks = for i <- 0 until tableSize yield {
      val nextCircularIndex = mod(i + 1)
      Signal.lift(phils(i), phils(nextCircularIndex))(calcFork(i.toString, nextCircularIndex.toString))
    }

    for i <- 0 until tableSize yield {
      val ownName = i.toString
      val vision = Signal.dynamic {
        forks(i).value match {
          case Taken(`ownName`) => Done
          case Taken(name)      => BlockedBy(name)
          case Free => forks(mod(i - 1)).value match {
              case Free        => Ready
              case Taken(name) => BlockedBy(name)
            }
        }
      }
      Seating(i, phils(i), forks(i), forks(mod(i - 1)), vision)
    }
  }

}

class OtherHalfDynamicPhilosopherTable(philosopherCount: Int, work: Long)(
    override implicit val engine: Interface
) extends PhilosopherTable(philosopherCount, work)(engine) {

  import engine.{Signal, Var}

  override def createTable(tableSize: Int): Seq[Seating] = {
    def mod(n: Int): Int = (n + tableSize) % tableSize

    val phils = for _ <- 0 until tableSize yield Var[Philosopher](Thinking)

    val forks = for i <- 0 until tableSize yield {
      val nextCircularIndex = mod(i + 1)
      Signal.dynamic {
        phils(i).value match {
          case Eating => Taken(i.toString)
          case Thinking =>
            phils(nextCircularIndex).value match {
              case Eating   => Taken(nextCircularIndex.toString)
              case Thinking => Free
            }
        }
      }

    }

    for i <- 0 until tableSize yield {
      val vision = Signal.lift(forks(i), forks(mod(i - 1)))(calcVision(i.toString))
      Seating(i, phils(i), forks(i), forks(mod(i - 1)), vision)
    }

  }

}
