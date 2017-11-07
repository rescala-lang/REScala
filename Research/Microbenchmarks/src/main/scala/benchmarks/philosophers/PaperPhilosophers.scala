package benchmarks.philosophers

import java.util.concurrent.{Executors, ThreadLocalRandom}

import rescala.core.{Engine, REName, Struct}
import rescala.parrp.Backoff

import scala.annotation.tailrec
import scala.concurrent.{Await, Future, TimeoutException}
import scala.concurrent.duration._
import scala.util.{Failure, Success}

class PaperPhilosophers[S <: Struct](val size: Int, val engine: Engine[S]) {
  import engine._

  sealed trait Philosopher
  case object Eating extends Philosopher
  case object Thinking extends Philosopher

  val phils = for(j <- 0 until size) yield
    Var[Philosopher](Thinking)

  sealed trait Fork
  case object Free extends Fork
  case class Taken(by: Int) extends Fork

  val forks = for(idx <- 0 until size) yield
    REName.named(s"fork($idx)"){ implicit! =>
      Signal[Fork] {
        val nextIdx = (idx + 1) % size
        (phils(idx)(), phils(nextIdx)()) match {
          case (Thinking, Thinking) => Free
          case (Eating, Thinking) => Taken(idx)
          case (Thinking, Eating) => Taken(nextIdx)
          case (Eating, Eating) => throw new AssertionError()
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
      Signal[Sight] {
        val prevIdx = (idx - 1 + size) % size
        forks(prevIdx)() match {
          case Free => forks(idx)() match {
            case Taken(neighbor) => Blocked(neighbor)
            case Free => Ready
          }
          case Taken(by) =>
            if (by == idx) {
              assert(forks(idx)() == Taken(idx))
              Done
            } else {
              Blocked(by)
            }
        }
      }
    }

  // "Static" Sight
//  val sights = for(idx <- 0 until size) yield
//    REName.named(s"sight($idx)") { implicit ! =>
//      Signal[Sight] {
//        val prevIdx = (idx - 1 + size) % size
//        (forks(prevIdx)(), forks(idx)()) match {
//          case (Free, Free) =>
//            Ready
//          case (Taken(left), Taken(right)) if left == idx && right == idx =>
//            Done
//          case (Taken(by), _) =>
//            assert(by != idx)
//            Blocked(by)
//          case (_, Taken(by)) =>
//            assert(by != idx)
//            Blocked(by)
//        }
//      }
//    }

  val sightChngs: Seq[Event[Sight]] =
    for(i <- 0 until size) yield sights(i).changed
  val successes = for(i <- 0 until size) yield
    sightChngs(i).filter(_ == Done)
  val anySuccess = successes.reduce(_ || _)
  val successCount: Signal[Int] =
    REName.named(s"successCount") { implicit ! =>
      anySuccess.fold(0) { (acc, _) => acc + 1 }
    }

  def maybeEat(idx: Int): Unit = {
    transaction(phils(idx)) { implicit t =>
      if(t.now(sights(idx)) == Ready) phils(idx).admit(Eating)
    }
  }
  def hasEaten(idx: Int): Boolean = {
    sights(idx).now == Done
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

  def total: Int = successCount.now
}

object PaperPhilosophers {
  def main(args: Array[String]): Unit = {
    val tableSize = if(args.length >= 1) Integer.parseInt(args(0)) else 5
    val threadCount = if(args.length >= 2) Integer.parseInt(args(1)) else tableSize
    val duration = if(args.length >= 3) Integer.parseInt(args(2)) else 0

    val engine = new rescala.fullmv.FullMVEngine(Duration.Zero, s"PaperPhilosophers($tableSize,$threadCount)")
    val table = new PaperPhilosophers(tableSize, engine)

    val continue: () => Boolean = if(duration == 0) {
      println("Running in interactive mode: press <Enter> to terminate.")
      () => System.in.available() <= 0
    } else {
      val end = System.currentTimeMillis() + duration
      () => System.currentTimeMillis() < end
    }
    def driver(idx: Int): Int = {
      var localCount = 0
      while(continue()) {
        table.eatRandomOnce(idx, threadCount)
        localCount += 1
      }
      localCount
    }

    val executor = Executors.newFixedThreadPool(threadCount)
    val execContext = scala.concurrent.ExecutionContext.fromExecutor(executor)
    val threads = for(i <- 0 until threadCount) yield Future { driver(i) }(execContext)

    while(continue()) { Thread.sleep(10) }
    val timeout = System.currentTimeMillis() + 500
    val scores = threads.map{ t =>
      Await.ready(t, (timeout - System.currentTimeMillis()).millis )
      t.value.get
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
    } else {
      val individualsSum = scores.map(_.get).sum
      if(table.total == individualsSum){
        println("Total score: " + table.total + " (matches individual scores' sum)")
      } else {
        println("Total score: " + table.total + " (differs from individual scores' sum of " + individualsSum + ")")
      }
    }

    val remainingTurns = engine.instances.size()
    if(remainingTurns != 0) println(remainingTurns + " turn instances were not garbage collected")
    val remainingLocks = engine.lockHost.instances.size()
    if(remainingLocks != 0) println(remainingLocks + " lock instances were not garbage collected")
  }
}
