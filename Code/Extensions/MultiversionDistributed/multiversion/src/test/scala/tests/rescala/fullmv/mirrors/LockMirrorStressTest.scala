package tests.rescala.fullmv.mirrors

import java.util.concurrent.atomic.AtomicReference

import org.scalatest.funsuite.AnyFunSuite
import rescala.fullmv._
import DistributedFullMVApi.{FullMVEngine, FullMVTurnLocalClone, SerializationGraphTracking, FullMVTurnImpl}
import rescala.parrp.Backoff
import tests.rescala.testtools.Spawn

import scala.annotation.tailrec
import scala.concurrent.TimeoutException
import scala.concurrent.duration.Duration
import scala.util.{Failure, Random, Success}

class LockMirrorStressTest extends AnyFunSuite {
  test("stress") {

    val numWorkers = 4

    val hosts = Array.tabulate(numWorkers)(i => new FullMVEngine(Duration.Zero, "stress-" + i))
    val turns = Array.tabulate(numWorkers) { i =>
      val turn = hosts(i).newTurn()
      turn.beginExecuting()
      new AtomicReference(turn)
    }
    val duration = 10000
    println(s"starting lock stress test " + (if (duration == 0) "until key press"
                                             else s"for ${duration / 1000} seconds..."))
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
              ownTurn = hosts(i).newTurn()
              ownTurn.beginExecuting()
              turns(i).set(ownTurn)
            } else {
              val pick    = random.nextInt(numWorkers)
              val backoff = new Backoff(maxBackoff = 100L * 1000L * 1000L)
              @tailrec def reTryLock(): Unit = {
                if (running) {
                  val turnOnLocalHost = FullMVTurnLocalClone(turns(pick).get, hosts(i))
                  if (turnOnLocalHost.phase < TurnPhase.Completed) {
                    SerializationGraphTracking.tryLock(turnOnLocalHost, ownTurn, UnlockedUnknown) match {
                      case LockedSameSCC(lock) => lock.asyncUnlock()
                      case _                   => backoff.backoff(); reTryLock()
                    }
                  } else reTryLock()
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
    while (running && (if (duration == 0) System.in.available() == 0 else System.currentTimeMillis() < timeout)) {
      Thread.sleep(50)
    }
    if (!running)
      println(s"Premature termination after ${(duration - (timeout - System.currentTimeMillis())) / 1000} seconds")
    running = false

    val finalTimeout = System.currentTimeMillis() + 500
    val scores       = threads.map(_.awaitTry(math.max(0, finalTimeout - System.currentTimeMillis())))
    println("lock stress test thread results:")
    println("\t" + scores.zipWithIndex.map { case (count, idx) => idx.toString + ": " + count }.mkString("\n\t"))
    scores.find {
      case Failure(ex: TimeoutException) => false
      case Failure(_)                    => true
      case Success(_)                    => false
    }.asInstanceOf[Option[Failure[_]]].foreach {
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
        hosts.foreach(host => println(s"$host orphan stats: ${host.cacheStatus}"))

        println(" == Orphan listing == ")
        hosts.foreach { host =>
          if (!host.instances.isEmpty || !host.lockHost.instances.isEmpty) {
            println(s"orphans on $host:")
            val it1 = host.instances.values().iterator()
            while (it1.hasNext) println("\t" + it1.next())
            val it2 = host.lockHost.instances.values().iterator()
            while (it2.hasNext) println("\t" + it2.next())
          }
        }

        assert(hosts.map(host => host.instances.size() + host.lockHost.instances.size()).sum === 0)
    }
  }
}
