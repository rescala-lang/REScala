package tests.rescala.fullmv

import java.util.concurrent.atomic.AtomicReference

import reactives.fullmv._
import tests.rescala.testtools.{IgnoreOnWindowsBecause, Spawn}

import scala.annotation.tailrec
import scala.concurrent.TimeoutException
import scala.concurrent.duration.Duration
import scala.util.{Failure, Random, Success}

class LockStressTest extends munit.FunSuite {
  // IgnoreOnWindowsBecause("this stackoverlfows in subsumable lock"))
  test("stress") {
    val host = new FullMVEngine(Duration.Zero, "lockStressTest")

    val numWorkers = 4

    val turns = Array.fill(numWorkers) {
      val turn = host.newTurn()
      turn.beginExecuting()
      new AtomicReference(turn)
    }
    val duration = 10000
    println(s"starting lock stress test for ${duration / 1000} seconds...")
    var running: Boolean = true
    val threads = Array.tabulate(numWorkers)(i =>
      Spawn {
        try {
          val random     = new Random()
          var ownTurn    = turns(i).get
          var iterations = 0L
          while (running) {
            if (random.nextInt(5) == 0) {
              ownTurn.completeExecuting()
              ownTurn = host.newTurn()
              ownTurn.beginExecuting()
              turns(i).set(ownTurn)
            } else {
              val pick = random.nextInt(numWorkers)
              @tailrec def reTryLock(): Unit = {
                if (running) SerializationGraphTracking.tryLock(turns(pick).get, ownTurn, UnlockedUnknown) match {
                  case LockedSameSCC(lock) => lock.asyncUnlock()
                  case _                   => reTryLock()
                }
              }
              reTryLock()
            }
            iterations += 1
          }
          iterations
        } catch {
          case t: Throwable =>
            running = false
            throw t
        }
      }
    )

    val timeout = System.currentTimeMillis() + duration
    while (running && System.currentTimeMillis() < timeout) {
      Thread.sleep(50)
    }
    if (!running)
      println(s"Premature termination after ${(duration - (timeout - System.currentTimeMillis())) / 1000} seconds")
    running = false

    val finalTimeout = System.currentTimeMillis() + 1000
    val scores       = threads.map(_.awaitTry(math.max(0, finalTimeout - System.currentTimeMillis())))
    println("lock stress test thread results:")
    println("\t" + scores.zipWithIndex.map { case (count, idx) => s"$idx: $count" }.mkString("\n\t"))
    scores.find {
      case Failure(ex: TimeoutException) => false
      case Failure(_)                    => true
      case Success(_)                    => false
    }.asInstanceOf[Option[Failure[?]]].foreach {
      case Failure(ex) =>
        ex.printStackTrace()
    }
    scores.foldLeft(Option(0L)) {
      case (None, _)                         => None
      case (Some(score), Failure(_))         => None
      case (Some(score), Success(moreScore)) => Some(score + moreScore)
    } match {
      case None =>
        println("no total and stats due to failures. state snapshot:")
        println(turns.zipWithIndex.map {
          case (t, idx) =>
            val turn = t.get
            s"$idx: $turn with ${turn.asInstanceOf[FullMVTurnImpl].subsumableLock.get}"
        }.mkString("\n"))
        fail("there were errors")
      case Some(sum) =>
        println(s"lock stress test totaled $sum iterations (individual scores: ${scores.mkString(", ")}")
        turns.foreach(_.get.completeExecuting())
        println(
          s"garbage stats: ${host.instances.size()} orphan turn instances and ${host.lockHost.instances.size()} orphan lock instances"
        )
        assertEquals(host.instances.size() + host.lockHost.instances.size(), 0)
    }
  }
}
