package tests.rescala.fullmv.transmitter

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import org.scalatest.FunSuite
import rescala.fullmv.transmitter.ReactiveTransmittable
import rescala.fullmv._
import retier.communicator.tcp.TCP
import retier.registry.{Binding, Registry}
import tests.rescala.testtools.Spawn

import scala.annotation.tailrec
import scala.concurrent.{Await, TimeoutException}
import scala.concurrent.duration._
import scala.util.{Failure, Success}

class XShapeSerializabilityTest extends FunSuite {
  case class Data[+T](name: String, data: T)
  case class Merge[+T](left: T, right: T)

  def isGlitched(v: Merge[Data[Merge[_]]]): Boolean = v.left.data != v.right.data

  class SideHost(name: String) extends FullMVEngine(10.second, name) {
    val registry = new Registry
    val port: Int = TransmitterTestsPortManagement.getFreePort()
    registry.listen(TCP(port))
    def shutdown() = registry.terminate()

    import ReactiveTransmittable._
    import io.circe.generic.auto._
    import rescala.fullmv.transmitter.CirceSerialization._
    implicit val host = this

    val sourceBinding = Binding[Signal[Data[Int]]]("source")
    val derivedBinding = Binding[Signal[Data[Merge[Data[Int]]]]]("derived")

    val counter = new AtomicInteger(0)
    def nextValue() = Data(name, counter.getAndIncrement)

    val source = Var(nextValue)
    def step() = source.set(nextValue)
    registry.bind(sourceBinding)(source)
  }

  test("X-shaped serializability") {
    val leftHost = new SideHost("left")
    val rightHost = new SideHost("right")
    try {
      val leftMerge = {
        import leftHost._
        val remoteRight = Await.result(registry.request(TCP("localhost", rightHost.port)), timeout)
        val sourceFromRight = Await.result(registry.lookup(sourceBinding, remoteRight), timeout)
        val merge = Signal{ Data("lmerge", Merge(source(), sourceFromRight())) }
        registry.bind(derivedBinding)(merge)
        merge
      }

      val rightMerge = {
        import rightHost._
        val remoteLeft = Await.result(registry.request(TCP("localhost", leftHost.port)), timeout)
        val sourceFromLeft = Await.result(registry.lookup(sourceBinding, remoteLeft), timeout)
        val merge = Signal{ Data("rmerge", Merge(sourceFromLeft(), source())) }
        registry.bind(derivedBinding)(merge)
        merge
      }

      object topHost extends FullMVEngine(10.second, "top") {
        val registry = new Registry

        import ReactiveTransmittable._
        import io.circe.generic.auto._
        import rescala.fullmv.transmitter.CirceSerialization._
        implicit val host = this

        val derivedBinding = Binding[Signal[Data[Merge[Data[Int]]]]]("derived")
      }
      import topHost._

      val remoteLeft = Await.result(registry.request(TCP("localhost", leftHost.port)), timeout)
      val mergeFromLeft = Await.result(registry.lookup(derivedBinding, remoteLeft), timeout)
      val remoteRight = Await.result(registry.request(TCP("localhost", rightHost.port)), timeout)
      val mergeFromRight = Await.result(registry.lookup(derivedBinding, remoteRight), timeout)
      val merge = Signal{ Merge(mergeFromLeft(), mergeFromRight()) }
      val violations = new AtomicReference[List[Merge[Data[Merge[Data[Int]]]]]](Nil)
      merge.observe { v =>
        if(isGlitched(v)) {
          @tailrec @inline def retryAdd(): Unit = {
            val before = violations.get
            if(!violations.compareAndSet(before, v :: before)) retryAdd()
          }
          retryAdd()
        }
      }

      assert(violations.get.isEmpty)
      assert(merge.now === Merge(
        Data("lmerge", Merge(Data("left", 0), Data("right", 0))),
        Data("rmerge", Merge(Data("left", 0), Data("right", 0)))
      ))

      leftHost.step()

      assert(violations.get.isEmpty)
      assert(merge.now === Merge(
        Data("lmerge", Merge(Data("left", 1), Data("right", 0))),
        Data("rmerge", Merge(Data("left", 1), Data("right", 0)))
      ))

      rightHost.step()

      assert(violations.get.isEmpty)
      assert(merge.now === Merge(
        Data("lmerge", Merge(Data("left", 1), Data("right", 1))),
        Data("rmerge", Merge(Data("left", 1), Data("right", 1)))
      ))


      val duration = 10000
      println(s"starting lock stress test " + (if(duration == 0) "until key press" else s"for ${duration / 1000} seconds..."))
      var running: Boolean = true
      def worker(host: SideHost) = Spawn {
        try {
          var iterations = 1
          while(running) {
            host.step()
            iterations += 1
            if(violations.get.nonEmpty) throw new RuntimeException(s"Found Violations after iteration $iterations")
          }
          iterations
        } catch {
          case t: Throwable =>
            running = false
            throw t
        }
      }
      val workerLeft = worker(leftHost)
      val workerRight = worker(rightHost)

      val workerTimeout = System.currentTimeMillis() + duration
      while(running && (if(duration == 0) System.in.available() == 0 else System.currentTimeMillis() < workerTimeout)) {
        Thread.sleep(50)
      }
      if(!running) println(s"Premature termination after ${(duration - (workerTimeout - System.currentTimeMillis())) / 1000} seconds")
      running = false

      val scoreLeft = workerLeft.await(500)
      val scoreRight = workerRight.await(500)
      val scores = Array(scoreLeft, scoreRight)
      println("lock stress test thread results:")
      println("\t" + scores.zipWithIndex.map { case (count, idx) => idx + ": " + count }.mkString("\n\t"))
      scores.find {
        case Failure(ex: TimeoutException) => false
        case Failure(_) => true
        case Success(_) => false
      }.asInstanceOf[Option[Failure[_]]].foreach {
        case Failure(ex) =>
          ex.printStackTrace()
      }
      scores.foldLeft(Option(0L)) {
        case (None, _) => None
        case (Some(score), Failure(_)) => None
        case (Some(score), Success(moreScore)) => Some(score + moreScore)
      } match {
        case None =>
          println("no total and stats due to failures. state snapshot:")
          println(s"sources: ${leftHost.source.now} and ${leftHost.source.now}")
          println(s"side merges: ${leftMerge.now} and ${rightMerge.now}")
          println(s"top merge: ${merge.now}")
          fail("there were errors")
        case Some(sum) =>
          println(s"lock stress test totaled $sum iterations (individual scores: ${scores.mkString(", ")}")
          assert(violations.get.isEmpty)
          assert(merge.now === Merge(
            Data("lmerge", Merge(Data("left", scoreLeft.get), Data("right", scoreRight.get))),
            Data("rmerge", Merge(Data("left", scoreLeft.get), Data("right", scoreRight.get)))
          ))

          val hosts = Array(leftHost, rightHost)
          hosts.foreach(host => println(s"$host orphan stats: ${host.cacheStatus}"))

          println(" == Orphan listing == ")
          hosts.foreach { host =>
            if(!host.instances.isEmpty || !host.lockHost.instances.isEmpty) {
              println(s"orphans on $host:")
              val it1 = host.instances.values().iterator()
              while (it1.hasNext) println("\t" + it1.next())
              val it2 = host.lockHost.instances.values().iterator()
              while (it2.hasNext) println("\t" + it2.next())
            }
          }

          assert(hosts.map(host => host.instances.size() + host.lockHost.instances.size()).sum === 0)
      }

    } finally {
      leftHost.shutdown()
      rightHost.shutdown()
    }
  }
}
