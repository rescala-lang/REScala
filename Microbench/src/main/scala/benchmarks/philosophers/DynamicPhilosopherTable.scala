package benchmarks.philosophers

import benchmarks.philosophers.PhilosopherTable._
import rescala.graph.Spores
import rescala.Signals.lift
import rescala.turns.{Engine, Turn}
import rescala.{Signals, Var}

class DynamicPhilosopherTable[S <: Spores](philosopherCount: Int, work: Long)(override implicit val engine: Engine[S, Turn[S]]) extends PhilosopherTable(philosopherCount, work)(engine) {

  override def createTable(tableSize: Int): Seq[Seating[S]] = {
    def mod(n: Int): Int = (n + tableSize) % tableSize

    val phils = for (i <- 0 until tableSize) yield Var[Philosopher, S](Thinking)

    val forks = for (i <- 0 until tableSize) yield {
      val nextCircularIndex = mod(i + 1)
      Signals.dynamic(phils(i), phils(nextCircularIndex)) { turn =>
        phils(i)(turn) match {
          case Eating => Taken(i.toString)
          case Thinking =>
            phils(nextCircularIndex)(turn) match {
              case Eating => Taken(nextCircularIndex.toString)
              case Thinking => Free
            }
        }
      }

    }

    for (i <- 0 until tableSize) yield {
      val ownName = i.toString
      val vision = Signals.dynamic(forks(i), forks(mod(i - 1))) { turn =>
        forks(i)(turn) match {
          case Taken(name) if name != ownName => BlockedBy(name)
          case Taken(`ownName`) => Done
          case Free => forks(mod(i - 1))(turn) match {
            case Free => Ready
            case Taken(name) => BlockedBy(name)
          }
        }
      }
      Seating(i, phils(i), forks(i), forks(mod(i - 1)), vision)
    }
  }

}

class HalfDynamicPhilosopherTable[S <: Spores](philosopherCount: Int, work: Long)(override implicit val engine: Engine[S, Turn[S]]) extends PhilosopherTable(philosopherCount, work)(engine) {

  override def createTable(tableSize: Int): Seq[Seating[S]] = {
    def mod(n: Int): Int = (n + tableSize) % tableSize

    val phils = for (i <- 0 until tableSize) yield Var[Philosopher, S](Thinking)

    val forks = for (i <- 0 until tableSize) yield {
      val nextCircularIndex = mod(i + 1)
      Signals.lift(phils(i), phils(nextCircularIndex))(calcFork(i.toString, nextCircularIndex.toString))
    }

    for (i <- 0 until tableSize) yield {
      val ownName = i.toString
      val vision = Signals.dynamic(forks(i), forks(mod(i - 1))) { turn =>
        forks(i)(turn) match {
          case Taken(name) if name != ownName => BlockedBy(name)
          case Taken(`ownName`) => Done
          case Free => forks(mod(i - 1))(turn) match {
            case Free => Ready
            case Taken(name) => BlockedBy(name)
          }
        }
      }
      Seating(i, phils(i), forks(i), forks(mod(i - 1)), vision)
    }
  }

}

class OtherHalfDynamicPhilosopherTable[S <: Spores](philosopherCount: Int, work: Long)(override implicit val engine: Engine[S, Turn[S]]) extends PhilosopherTable(philosopherCount, work)(engine) {

  override def createTable(tableSize: Int): Seq[Seating[S]] = {
    def mod(n: Int): Int = (n + tableSize) % tableSize

    val phils = for (i <- 0 until tableSize) yield Var[Philosopher, S](Thinking)

    val forks = for (i <- 0 until tableSize) yield {
      val nextCircularIndex = mod(i + 1)
      Signals.dynamic(phils(i), phils(nextCircularIndex)) { turn =>
        phils(i)(turn) match {
          case Eating => Taken(i.toString)
          case Thinking =>
            phils(nextCircularIndex)(turn) match {
              case Eating => Taken(nextCircularIndex.toString)
              case Thinking => Free
            }
        }
      }

    }

    for (i <- 0 until tableSize) yield {
      val vision = lift(forks(i), forks(mod(i - 1)))(calcVision(i.toString))
      Seating(i, phils(i), forks(i), forks(mod(i - 1)), vision)
    }

  }

}
