package benchmarks.philosophers

import benchmarks.philosophers.PhilosopherTable._
import rescala.core.{Scheduler, Struct}
import rescala.reactives.{Signals, Var}

class DynamicPhilosopherTable[S <: Struct](philosopherCount: Int, work: Long)(override implicit val engine: Scheduler[S]) extends PhilosopherTable(philosopherCount, work)(engine) {

  import engine.Signal

  override def createTable(tableSize: Int): Seq[Seating[S]] = {
    def mod(n: Int): Int = (n + tableSize) % tableSize

    val phils = for (i <- 0 until tableSize) yield Var[Philosopher, S](Thinking)

    val forks = for (i <- 0 until tableSize) yield {
      val nextCircularIndex = mod(i + 1)
      Signal {
        phils(i)() match {
          case Eating => Taken(i.toString)
          case Thinking =>
            phils(nextCircularIndex)() match {
              case Eating => Taken(nextCircularIndex.toString)
              case Thinking => Free
            }
        }
      }

    }

    for (i <- 0 until tableSize) yield {
      val ownName = i.toString
      val vision = Signal {
        forks(i)() match {
          case Taken(name) if name != ownName => BlockedBy(name)
          case Taken(`ownName`) => Done
          case Free => forks(mod(i - 1))() match {
            case Free => Ready
            case Taken(name) => BlockedBy(name)
          }
        }
      }
      Seating(i, phils(i), forks(i), forks(mod(i - 1)), vision)
    }
  }

}

class HalfDynamicPhilosopherTable[S <: Struct](philosopherCount: Int, work: Long)(override implicit val engine: Scheduler[S]) extends PhilosopherTable(philosopherCount, work)(engine) {

  import engine.Signal

  override def createTable(tableSize: Int): Seq[Seating[S]] = {
    def mod(n: Int): Int = (n + tableSize) % tableSize

    val phils = for (i <- 0 until tableSize) yield Var[Philosopher, S](Thinking)

    val forks = for (i <- 0 until tableSize) yield {
      val nextCircularIndex = mod(i + 1)
      Signals.lift(phils(i), phils(nextCircularIndex))(calcFork(i.toString, nextCircularIndex.toString))
    }

    for (i <- 0 until tableSize) yield {
      val ownName = i.toString
      val vision = Signal {
        forks(i)() match {
          case Taken(name) if name != ownName => BlockedBy(name)
          case Taken(`ownName`) => Done
          case Free => forks(mod(i - 1))() match {
            case Free => Ready
            case Taken(name) => BlockedBy(name)
          }
        }
      }
      Seating(i, phils(i), forks(i), forks(mod(i - 1)), vision)
    }
  }

}

class OtherHalfDynamicPhilosopherTable[S <: Struct](philosopherCount: Int, work: Long)(override implicit val engine: Scheduler[S]) extends PhilosopherTable(philosopherCount, work)(engine) {

  import engine.Signal

  override def createTable(tableSize: Int): Seq[Seating[S]] = {
    def mod(n: Int): Int = (n + tableSize) % tableSize

    val phils = for (i <- 0 until tableSize) yield Var[Philosopher, S](Thinking)

    val forks = for (i <- 0 until tableSize) yield {
      val nextCircularIndex = mod(i + 1)
      Signal {
        phils(i)() match {
          case Eating => Taken(i.toString)
          case Thinking =>
            phils(nextCircularIndex)() match {
              case Eating => Taken(nextCircularIndex.toString)
              case Thinking => Free
            }
        }
      }

    }

    for (i <- 0 until tableSize) yield {
      val vision = Signals.lift(forks(i), forks(mod(i - 1)))(calcVision(i.toString))
      Seating(i, phils(i), forks(i), forks(mod(i - 1)), vision)
    }

  }

}
