package benchmarks.philosophers

import reactives.core.{CreationTicket, ReInfo, ReSource}
import reactives.SelectedScheduler.State as BundleState

import reactives.parrp.Backoff

import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.{Executors, ThreadLocalRandom}
import scala.annotation.tailrec
import scala.concurrent.duration.*
import scala.concurrent.{Await, Future, TimeoutException}
import scala.util.{Failure, Success, Try}
import reactives.default.*

sealed trait Dynamicity
object Dynamicity {
  case object Static     extends Dynamicity
  case object SemiStatic extends Dynamicity
  case object Dynamic    extends Dynamicity
}
abstract class PaperPhilosophers(val size: Int, val engine: Any, dynamicity: Dynamicity) {

  sealed trait Philosopher

  case object Eating extends Philosopher

  case object Thinking extends Philosopher

  val phils =
    for idx <- 0 until size yield {
      Var[Philosopher](Thinking)(using CreationTicket.fromName(s"phil(${idx + 1})"))
    }

  sealed trait Fork

  case object Free extends Fork

  case class Taken(by: Int) extends Fork

  val forks =
    for idx <- 0 until size yield {
      implicit val ct: CreationTicket[BundleState] = (CreationTicket.fromName(s"fork(${idx + 1})"))
      Signal.dynamic[Fork] {
        val nextIdx = (idx + 1) % size
        (phils(idx).value, phils(nextIdx).value) match {
          case (Thinking, Thinking) => Free
          case (Eating, Thinking)   => Taken(idx)
          case (Thinking, Eating)   => Taken(nextIdx)
          case (Eating, Eating)     => throw new AssertionError(s"fork ${idx + 1} double use")
        }
      }
    }

  sealed trait Sight

  case object Done extends Sight

  case class Blocked(by: Int) extends Sight

  case object Ready extends Sight

  // Dynamic Sight
  val sights =
    for avoidStaticOptimization <- 0 until size yield {
      dynamicity match {
        case Dynamicity.Dynamic => Signal.dynamic[Sight] {
            val idx     = avoidStaticOptimization
            val prevIdx = (idx - 1 + size) % size
            forks(prevIdx).value match {
              case Free =>
                forks(idx).value match {
                  case Taken(neighbor) => Blocked(neighbor)
                  case Free            => Ready
                }
              case Taken(by) =>
                if by == idx then {
                  assert(forks(idx).value == Taken(idx), s"sight ${idx + 1} glitched")
                  Done
                } else {
                  Blocked(by)
                }
            }
          }
        case Dynamicity.SemiStatic => Signal.dynamic[Sight] {
            val idx     = avoidStaticOptimization
            val prevIdx = (idx - 1 + size) % size
            computeForkStatic(idx, (forks(prevIdx).value, forks(idx).value))
          }
        case Dynamicity.Static =>
          val idx       = avoidStaticOptimization
          val prevIdx   = (idx - 1 + size) % size
          val leftFork  = forks(prevIdx)
          val rightFork = forks(idx)
          Signal[Sight] {
            computeForkStatic(idx, (leftFork.value, rightFork.value))
          }
      }

    }

  private def computeForkStatic(idx: Int, forkStates: (Fork, Fork)) = {
    forkStates match {
      case (Free, Free) =>
        Ready
      case (Taken(left), Taken(right)) if left == idx && right == idx =>
        Done
      case (Taken(by), _) =>
        assert(by != idx, s"sight ${idx + 1} glitched 1")
        Blocked(by)
      case (_, Taken(by)) =>
        assert(by != idx, s"sight ${idx + 1} glitched 2")
        Blocked(by)
    }
  }

  val sightChngs: Seq[Event[Sight]] =
    for i <- 0 until size yield sights(i).changed
  val successes = for i <- 0 until size yield sightChngs(i).filter(_ == Done)

  def manuallyLocked[T](idx: Int)(f: => T): T = synchronized { f }

  def maybeEat(idx: Int): Unit = {
    transaction(phils(idx)) { t ?=>
      if t.now(sights(idx)) == Ready then phils(idx).admit(Eating)
    }
  }
  def hasEaten(idx: Int): Boolean = {
    sights(idx).readValueOnce == Done
  }
  def rest(idx: Int): Unit = {
    phils(idx).set(Thinking)
  }

  def eatRandomOnce(threadIndex: Int, threadCount: Int): Unit = {
    val seatsServed  = size / threadCount + (if threadIndex < size % threadCount then 1 else 0)
    val seating: Int = threadIndex + ThreadLocalRandom.current().nextInt(seatsServed) * threadCount
    eatOnce(seating)
  }

  def eatOnce(seating: Int) = {
    val bo = new Backoff()
    @tailrec def retryEating(): Unit = {
      maybeEat(seating)
      if hasEaten(seating) then {
        rest(seating)
      } else {
        bo.backoff()
        retryEating()
      }
    }
    retryEating()
  }

  def total: Int
}

trait EventPyramidTopper {
  self: PaperPhilosophers =>
  import engine.*

  val anySuccess = successes.reduce(_ || _)
  val successCount: Signal[Int] =
    anySuccess.fold(0) { (acc, _) => acc + 1 }(using CreationTicket.fromName(s"successCount"))
  override def total: Int = successCount.readValueOnce
}

trait IndividualCounts {
  self: PaperPhilosophers =>
  import engine.*

  val individualCounts: Seq[Signal[Int]] =
    for idx <- 0 until size yield {
      successes(idx).fold(0) { (acc, _) => acc + 1 }(using CreationTicket.fromName(s"count(${idx + 1})"))
    }
}

trait NoTopper extends IndividualCounts {
  self: PaperPhilosophers =>

  val locks = Array.fill(size) { new ReentrantLock() }
  override def manuallyLocked[T](idx: Int)(f: => T): T = {
    val (lock1, lock2, lock3) =
      if idx == 0 then {
        (locks(0), locks(1), locks(size - 1))
      } else if idx == size - 1 then {
        (locks(0), locks(size - 2), locks(size - 1))
      } else {
        (locks(idx - 1), locks(idx), locks(idx + 1))
      }
    lock1.lock(); lock2.lock(); lock3.lock()
    try {
      f
    } finally {
      lock1.unlock(); lock2.unlock(); lock3.unlock()
    }
  }

  override def total: Int = individualCounts.map(_.readValueOnce).sum
}

trait SignalPyramidTopper extends IndividualCounts {
  self: PaperPhilosophers =>
  import engine.*

  val successCount: Signal[Int] =
    individualCounts.reduce { (a, b) =>
      {
        Signal { a.value + b.value }(using (CreationTicket.fromName(s"sumUpTo($b)")))
      }
    }
  override def total: Int = successCount.readValueOnce
}

trait SingleFoldTopper {
  self: PaperPhilosophers =>
  import engine.*

  val successCount: Signal[Int] = Fold(0)(successes.map(s => s branch { v => Fold.current[Int] + 1 })*)
  override def total: Int       = successCount.readValueOnce
}

trait ManualLocking extends PaperPhilosophers {
  override def maybeEat(idx: Int): Unit = {
    manuallyLocked(idx) {
      super.maybeEat(idx)
    }
  }

  override def rest(idx: Int): Unit = {
    manuallyLocked(idx) {
      super.rest(idx)
    }
  }
}

object PaperPhilosophers {
  def main(args: Array[String]): Unit = {
    val tableSize   = if args.length >= 1 then Integer.parseInt(args(0)) else 5
    val threadCount = if args.length >= 2 then Integer.parseInt(args(1)) else tableSize
    val duration    = if args.length >= 3 then Integer.parseInt(args(2)) else 0

    object engine extends reactives.fullmv.FullMVEngine(Duration.Zero, s"PaperPhilosophers($tableSize,$threadCount)")
    val table =
      new PaperPhilosophers(tableSize, reactives.default, Dynamicity.Dynamic) with SignalPyramidTopper
//    implicit val engine = rescala.levelbased.LevelBasedPropagationEngines.unmanaged
//    val table = new PaperPhilosophers(tableSize, engine, Dynamicity.Static) with NoTopper[rescala.levelbased.SimpleStruct] with ManualLocking[rescala.levelbased.SimpleStruct]

//    println("====================================================================================================")

    val continue: () => Boolean =
      if duration == 0 then {
        println("Running in interactive mode: press <Enter> to terminate.")
        () => System.in.available() <= 0
      } else {
        println(s"Running for ${duration / 1000} seconds...")
        val end = System.currentTimeMillis() + duration
        () => System.currentTimeMillis() < end
      }

    @volatile var abort: Boolean = false
    def driver(idx: Int): Int = {
      try {
        var localCount = 0
        while !abort && continue() do {
          table.eatRandomOnce(idx, threadCount)
          localCount += 1
        }
        localCount
      } catch {
        case t: Throwable =>
          println(s"Thread ${idx + 1} setting abort after error!")
          abort = true
          throw t
      }
    }

    val executor    = Executors.newFixedThreadPool(threadCount)
    val execContext = scala.concurrent.ExecutionContext.fromExecutor(executor)
    val threads     = for i <- 0 until threadCount yield Future { driver(i) }(execContext)

    while threads.exists(!_.isCompleted) && !abort && continue() do { Thread.sleep(10) }
    val timeout = System.currentTimeMillis() + 3000
    val scores = threads.map { t =>
      Try { Await.result(t, (timeout - System.currentTimeMillis()).millis) }
    }
    executor.shutdown()

    if scores.exists(_.isFailure) then {
      println("Philosophers done with failures:")
      scores.zipWithIndex.foreach {
        case (Failure(_: TimeoutException), idx) =>
          System.err.println(f"${idx + 1}%4d: timed out")
        case (f @ Failure(ex), idx) =>
          System.err.print(f"${idx + 1}%4d: ")
          ex.printStackTrace()
        case (Success(_), _) => // ignore
      }
      println("There were failures -> not accessing total score; individual threads summary:")
      println(
        "\t" + scores.zipWithIndex.map { case (count, idx) => (idx + 1).toString + ": " + count }.mkString("\n\t")
      )
    } else {
      println("Philosophers done. Individual threads' scores:")
      val individualsSum = scores.map(_.get).sum
      println(
        "\t" + scores.zipWithIndex.map { case (count, idx) => (idx + 1).toString + ": " + count }.mkString("\n\t")
      )
      if table.total == individualsSum then {
        println("Total score: " + table.total + " (matches individual scores' sum)")
      } else {
        println("Total score: " + table.total + " (differs from individual scores' sum of " + individualsSum + ")")
      }
    }
  }
}
