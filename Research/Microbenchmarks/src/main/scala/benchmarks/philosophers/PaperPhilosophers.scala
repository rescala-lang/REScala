package benchmarks.philosophers

import java.util.concurrent.{Executors, ThreadLocalRandom}

import rescala.core.{Scheduler, Pulse, REName, Struct}
import rescala.fullmv.FullMVStruct
import rescala.parrp.Backoff

import scala.annotation.tailrec
import scala.concurrent.{Await, Future, TimeoutException}
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

abstract class PaperPhilosophers[S <: Struct](val size: Int, val engine: Scheduler[S], dynamicEdgeChanges: Boolean) {
  import engine._

  sealed trait Philosopher
  case object Eating extends Philosopher
  case object Thinking extends Philosopher

  val phils = for(idx <- 0 until size) yield
    REName.named(s"phil($idx)") { implicit! =>
      Var[Philosopher](Thinking)
    }

  sealed trait Fork
  case object Free extends Fork
  case class Taken(by: Int) extends Fork

  val forks = for(idx <- 0 until size) yield
    REName.named(s"fork($idx)"){ implicit! =>
      Signal.dynamic[Fork] {
        val nextIdx = (idx + 1) % size
        (phils(idx)(), phils(nextIdx)()) match {
          case (Thinking, Thinking) => Free
          case (Eating, Thinking) => Taken(idx)
          case (Thinking, Eating) => Taken(nextIdx)
          case (Eating, Eating) => throw new AssertionError(s"fork $idx double use")
        }
      }
    }

  sealed trait Sight
  case object Done extends Sight
  case class Blocked(by: Int) extends Sight
  case object Ready extends Sight

  // Dynamic Sight
  val sights = for(idx <- 0 until size) yield
    REName.named(s"sight($idx)") { implicit ! =>
      Signal.dynamic[Sight] {
        val prevIdx = (idx - 1 + size) % size
        if(dynamicEdgeChanges) {
          forks(prevIdx)() match {
            case Free => forks(idx)() match {
              case Taken(neighbor) => Blocked(neighbor)
              case Free => Ready
            }
            case Taken(by) =>
              if (by == idx) {
                assert(forks(idx)() == Taken(idx), s"sight $idx glitched")
                Done
              } else {
                Blocked(by)
              }
          }
        } else {
          (forks(prevIdx)(), forks(idx)()) match {
            case (Free, Free) =>
              Ready
            case (Taken(left), Taken(right)) if left == idx && right == idx =>
              Done
            case (Taken(by), _) =>
              assert(by != idx, s"sight $idx glitched 1")
              Blocked(by)
            case (_, Taken(by)) =>
              assert(by != idx, s"sight $idx glitched 2")
              Blocked(by)
          }
        }
      }
    }

  val sightChngs: Seq[Event[Sight]] =
    for(i <- 0 until size) yield sights(i).changed
  val successes = for(i <- 0 until size) yield
    sightChngs(i).filter(_ == Done)

  def maybeEat(idx: Int): Unit = {
    transaction(phils(idx)) { implicit t =>
      if(t.now(sights(idx)) == Ready) phils(idx).admit(Eating)
    }
  }
  def hasEaten(idx: Int): Boolean = {
    sights(idx).readValueOnce == Done
  }
  def rest(idx: Int): Unit = {
    phils(idx).set(Thinking)
  }

  def eatRandomOnce(threadIndex: Int, threadCount: Int): Unit = {
    val seatsServed = size / threadCount + (if (threadIndex < size % threadCount) 1 else 0)
    val seating: Int = threadIndex + ThreadLocalRandom.current().nextInt(seatsServed) * threadCount
    val bo = new Backoff()
    @tailrec def retryEating(): Unit = {
      maybeEat(seating)
      if(hasEaten(seating)) {
        rest(seating)
      } else {
        bo.backoff()
        retryEating()
      }
    }
    retryEating()
  }

  // To be implemented by your choice of topper (see below)
  val successCount: Signal[Int]

  def total: Int = successCount.readValueOnce
}

trait EventTopper[S <: Struct] {
  self: PaperPhilosophers[S] =>
  import engine._

  val anySuccess = successes.reduce(_ || _)
  override val successCount: Signal[Int] =
    REName.named(s"successCount") { implicit ! =>
      anySuccess.fold(0) { (acc, _) => acc + 1 }
    }
}

trait SignalTopper[S <: Struct] {
  self: PaperPhilosophers[S] =>
  import engine._

  val individualCounts: Seq[Signal[Int]] =
    for(i <- 0 until size) yield
      REName.named(s"count($i)") { implicit ! =>
        successes(i).fold(0) { (acc, _) => acc + 1 }
      }
  override val successCount: Signal[Int] =
    individualCounts.reduce{ (a, b) =>
      REName.named(s"sumUpTo($b)") { implicit ! =>
        Signal { a() + b() }
      }
    }
}

trait TransposeTopper[S <: Struct] {
  self: PaperPhilosophers[S] =>
  import engine._

//  val individualCounts: Seq[Signal[Int]] =
//    for(i <- 0 until size) yield
//      REName.named(s"count($i)") { implicit ! =>
//        successes(i).fold(0) { (acc, _) => acc + 1 }
//      }
//  override val successCount: Signal[Int] = Signal {
//    individualCounts.map(_()).sum
////    individualCounts.foldLeft(0){ (sum, signal) => sum + signal() }
//  }


  override val successCount: Signal[Int] = Events.fold(successes.toSet[ReSource], 0){(ticket, before) => before() + 1}
}

object PaperPhilosophers {
  def main(args: Array[String]): Unit = {
    val tableSize = if(args.length >= 1) Integer.parseInt(args(0)) else 5
    val threadCount = if(args.length >= 2) Integer.parseInt(args(1)) else tableSize
    val duration = if(args.length >= 3) Integer.parseInt(args(2)) else 0

    implicit val engine = new rescala.fullmv.FullMVEngine(Duration.Zero, s"PaperPhilosophers($tableSize,$threadCount)")
    val table = new PaperPhilosophers(tableSize, engine, dynamicEdgeChanges = true) with TransposeTopper[FullMVStruct]

//    println("====================================================================================================")

    val continue: () => Boolean = if(duration == 0) {
      println("Running in interactive mode: press <Enter> to terminate.")
      () => System.in.available() <= 0
    } else {
      val end = System.currentTimeMillis() + duration
      () => System.currentTimeMillis() < end
    }

    @volatile var abort: Boolean = false
    def driver(idx: Int): Int = {
      try {
        var localCount = 0
        while(!abort && continue()) {
          table.eatRandomOnce(idx, threadCount)
          localCount += 1
        }
        localCount
      } catch {
        case t: Throwable =>
          abort = true
          throw t
      }
    }

    val executor = Executors.newFixedThreadPool(threadCount)
    val execContext = scala.concurrent.ExecutionContext.fromExecutor(executor)
    val threads = for(i <- 0 until threadCount) yield Future { driver(i) }(execContext)

    while(threads.exists(!_.isCompleted) && continue()) { Thread.sleep(10) }
    val timeout = System.currentTimeMillis() + 3000
    val scores = threads.map{ t =>
      Try { Await.result(t, (timeout - System.currentTimeMillis()).millis ) }
    }
    executor.shutdown()

    println("Philosophers done. Individual scores:")
    println("\t" + scores.zipWithIndex.map { case (count, idx) => idx + ": " + count }.mkString("\n\t"))
    scores.find {
      case Failure(ex: TimeoutException) => false
      case Failure(_) => true
      case Success(_) => false
    }.asInstanceOf[Option[Failure[_]]].foreach {
      case Failure(ex) =>
        ex.printStackTrace()
    }
    if(scores.exists(_.isFailure)) {
      println("There were failures -> not accessing total score")
      println("Thread pool state: "+engine.threadPool)
    } else {
      val individualsSum = scores.map(_.get).sum
      if(table.total == individualsSum){
        println("Total score: " + table.total + " (matches individual scores' sum)")
      } else {
        println("Total score: " + table.total + " (differs from individual scores' sum of " + individualsSum + ")")
      }
    }
    println(engine.instances.size() + " turn instances remain.")
    println(engine.lockHost.instances.size() + " lock instances remain.")
  }
}
