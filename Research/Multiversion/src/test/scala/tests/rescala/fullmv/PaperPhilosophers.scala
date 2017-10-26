package tests.rescala.fullmv

import rescala.testhelper.Spawn

import scala.concurrent.TimeoutException
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

object PaperPhilosophers {

  def main(args: Array[String]): Unit = {
    val SIZE = if(args.length >= 1) Integer.parseInt(args(0)) else 5

    val engine = new rescala.fullmv.FullMVEngine(Duration.Zero, "PaperPhilosophers")
    import engine._
    //import rescala._

    sealed trait Philosopher
    case object Eating extends Philosopher
    case object Thinking extends Philosopher

    val phils = for(j <- 0 until SIZE) yield
      Var[Philosopher](Thinking)

    sealed trait Fork
    case object Free extends Fork
    case class Taken(by: Int) extends Fork

    val forks = for(idx <- 0 until SIZE) yield
      Signal[Fork] {
        val nextIdx = (idx + 1) % SIZE
        (phils(idx)(), phils(nextIdx)()) match {
          case (Thinking, Thinking) => Free
          case (Eating, Thinking) => Taken(idx)
          case (Thinking, Eating) => Taken(nextIdx)
          case (Eating, Eating) => throw new AssertionError()
        }
      }

    sealed trait Sight
    case object Done extends Sight
    case class Blocked(by: Int) extends Sight
    case object Ready extends Sight
/*
    val sights = for(idx <- 0 until SIZE) yield
      Signal[Sight] {
        val prevIdx = (idx - 1 + SIZE) % SIZE
        forks(prevIdx)() match {
          case Free => forks(idx)() match {
            case Taken(neighbor) => Blocked(neighbor)
            case Free => Ready
          }
          case Taken(by) =>
            if(by == idx) {
              assert(forks(idx)() == Taken(idx))
              Done
            } else {
              Blocked(by)
            }
        }
      }
*/
    val sights = for(idx <- 0 until SIZE) yield
      Signal[Sight] {
        val prevIdx = (idx - 1 + SIZE) % SIZE
        (forks(prevIdx)(), forks(idx)()) match {
          case(Free, Free) =>
            Ready
          case (Taken(left), Taken(right)) if left == idx && right == idx =>
            Done
          case (Taken(by), _) =>
            assert(by != idx)
            Blocked(by)
          case (_, Taken(by)) =>
            assert(by != idx)
            Blocked(by)
        }
      }

    val sightChngs: Seq[Event[Sight]] =
      for(i <- 0 until SIZE) yield sights(i).changed
    val successes = for(i <- 0 until SIZE) yield
      sightChngs(i).filter(_ == Done)
    val anySuccess = successes.reduce(_ || _)
    val successCount: Signal[Int] =
      anySuccess.fold(0) { (acc, _) => acc + 1 }

    val end = System.currentTimeMillis() + 10000
    def driver(idx: Int): Int = {
      var localCount = 0
      while(System.currentTimeMillis() < end) {
        transaction(phils(idx)) { implicit t =>
          if(t.now(sights(idx)) == Ready) phils(idx).admit(Eating)
        }
        if(sights(idx).now == Done) {
          localCount += 1
          phils(idx).set(Thinking)
        }
      }
      localCount
    }

    val threads = for(i <- 0 until SIZE) yield Spawn { driver(i) }
    while(System.currentTimeMillis() < end) Thread.sleep(end - System.currentTimeMillis())
    val scores = threads.map(t => Try { t.join(end + 500 - System.currentTimeMillis()) })

    println("Philosophers done. Individual scores:")
    println("\t" + scores.zipWithIndex.map { case (count, idx) => idx + ": " + count }.mkString("\n\t"))
    scores.find(_ match {
      case Failure(ex: TimeoutException) => false
      case Failure(_) => true
      case Success(_) => false
    }).foreach {
      case Failure(ex) =>
        ex.printStackTrace()
    }
    if(scores.exists(_.isFailure)) {
      println("There were failures -> not accessing total score")
    } else {
      println("Total score: " + successCount.now)
    }

    assert(engine.instances.size() == 0, "some turn instances were not garbage collected")
    assert(engine.lockHost.instances.size() == 0, "some lock instances were not garbage collected")
  }
}
