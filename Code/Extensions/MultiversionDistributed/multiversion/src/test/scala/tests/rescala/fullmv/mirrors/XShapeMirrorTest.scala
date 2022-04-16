package tests.rescala.fullmv.mirrors

import org.scalatest.funsuite.AnyFunSuite
import rescala.fullmv.DistributedFullMVApi.{FullMVEngine, ReactiveLocalClone, Signal, Var}
import tests.rescala.testtools.Spawn

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import scala.annotation.tailrec
import scala.concurrent.TimeoutException
import scala.concurrent.duration._
import scala.util.{Failure, Success}

class XShapeMirrorTest extends AnyFunSuite {
  case class Data[+T](name: String, data: T)
  case class Merge[+T](left: T, right: T)

  def isGlitched(v: Merge[Data[Merge[_]]]): Boolean = v.left.data != v.right.data

  val ports = new AtomicInteger(1099)
  class SideHost(name: String) extends FullMVEngine(Duration.Zero, name) {
    val counter     = new AtomicInteger(0)
    def nextValue() = Data(name, counter.getAndIncrement)

    val source = Var(nextValue())
    def step() = source.set(nextValue())
  }

  test("X-shaped serializability") {
    val leftHost  = new SideHost("left")
    val rightHost = new SideHost("right")
    val leftMerge = {
      import leftHost._
      val sourceFromRight = ReactiveLocalClone(rightHost.source, leftHost)
      Signal { Data("lmerge", Merge(source(), sourceFromRight())) }
    }

    val rightMerge = {
      import rightHost._
      val sourceFromLeft = ReactiveLocalClone(leftHost.source, rightHost)
      Signal { Data("rmerge", Merge(sourceFromLeft(), source())) }
    }

    object topHost extends FullMVEngine(Duration.Zero, "top")

    val mergeFromLeft  = ReactiveLocalClone(leftMerge, topHost)
    val mergeFromRight = ReactiveLocalClone(rightMerge, topHost)
    val merge          = Signal { Merge(mergeFromLeft(), mergeFromRight()) }
    val violations     = new AtomicReference[List[Merge[Data[Merge[Data[Int]]]]]](Nil)
    merge.observe { v =>
      if (isGlitched(v)) {
        @tailrec @inline def retryAdd(): Unit = {
          val before = violations.get
          if (!violations.compareAndSet(before, v :: before)) retryAdd()
        }
        retryAdd()
      }
    }

    assert(violations.get.isEmpty)
    assert(merge.readValueOnce === Merge(
      Data("lmerge", Merge(Data("left", 0), Data("right", 0))),
      Data("rmerge", Merge(Data("left", 0), Data("right", 0)))
    ))

    leftHost.step()

    assert(violations.get.isEmpty)
    assert(merge.readValueOnce === Merge(
      Data("lmerge", Merge(Data("left", 1), Data("right", 0))),
      Data("rmerge", Merge(Data("left", 1), Data("right", 0)))
    ))

    rightHost.step()

    assert(violations.get.isEmpty)
    assert(merge.readValueOnce === Merge(
      Data("lmerge", Merge(Data("left", 1), Data("right", 1))),
      Data("rmerge", Merge(Data("left", 1), Data("right", 1)))
    ))

    val duration = 10000
    println(s"starting X-Shape mirror stress test " + (if (duration == 0) "until key press"
                                                       else s"for ${duration / 1000} seconds..."))
    var running: Boolean = true
    def worker(host: SideHost) =
      Spawn {
        try {
          var iterations = 1
          while (running) {
            host.step()
            iterations += 1
            if (violations.get.nonEmpty) throw new RuntimeException(s"Found Violations after iteration $iterations")
          }
          iterations
        } catch {
          case t: Throwable =>
            running = false
            throw t
        }
      }
    val workerLeft  = worker(leftHost)
    val workerRight = worker(rightHost)

    val workerTimeout = System.currentTimeMillis() + duration
    while (running && (if (duration == 0) System.in.available() == 0 else System.currentTimeMillis() < workerTimeout)) {
      Thread.sleep(50)
    }
    if (!running)
      println(
        s"Premature termination after ${(duration - (workerTimeout - System.currentTimeMillis())) / 1000} seconds"
      )
    running = false

    val scoreLeft  = workerLeft.awaitTry(500)
    val scoreRight = workerRight.awaitTry(500)
    val scores     = Array(scoreLeft, scoreRight)
    println("X-Shape mirror stress test thread results:")
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
        println(s"sources: ${leftHost.source.readValueOnce} and ${leftHost.source.readValueOnce}")
        println(s"side merges: ${leftMerge.readValueOnce} and ${rightMerge.readValueOnce}")
        println(s"top merge: ${merge.readValueOnce}")
        fail("there were errors")
      case Some(sum) =>
        println(s"X-Shape mirror stress test totaled $sum iterations (individual scores: ${scores.mkString(", ")}")
        assert(violations.get.isEmpty)
        assert(merge.readValueOnce === Merge(
          Data("lmerge", Merge(Data("left", scoreLeft.get), Data("right", scoreRight.get))),
          Data("rmerge", Merge(Data("left", scoreLeft.get), Data("right", scoreRight.get)))
        ))

        val hosts = Array(leftHost, rightHost)
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
