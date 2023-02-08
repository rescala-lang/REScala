package tests.rescala.fullmv.transmitter

import org.scalatest.funsuite.AnyFunSuite
import rescala.fullmv._
import DistributedFullMVApi.{FullMVEngine, ReactiveTransmittable, Signal, Var}
import loci.communicator.tcp.TCP
import loci.registry.{Binding, Registry}
import tests.rescala.testtools.Spawn

import com.github.plokhotnyuk.jsoniter_scala.macros._
import com.github.plokhotnyuk.jsoniter_scala.core._

import scala.concurrent.{Await, TimeoutException}
import scala.concurrent.duration._
import scala.util.{Failure, Success}

class XShapeSerializabilityTest extends AnyFunSuite {
  test("X-shaped serializability") {
    assert(XShapeSerializabilityTest.run(10000) === null)
  }
}

object XShapeSerializabilityTest {

  case class Data[+T](name: String, data: T)
  case class Merge[+T](left: T, right: T)

  import loci.transmitter._

  implicit def dataTransmittable[T, I, U](implicit
      transmittable: Transmittable[(String, T), I, (String, U)]
  ): DelegatingTransmittable[Data[T], I, Data[U]] {
    type Delegates = transmittable.Type
  } =
    DelegatingTransmittable(
      provide = (value, context) => context.delegate(value.name -> value.data),
      receive = (value, context) => (Data[U] _).tupled(context.delegate(value))
    )

  implicit def mergeTransmittable[T, I, U](implicit
      transmittable: Transmittable[(T, T), I, (U, U)]
  ): DelegatingTransmittable[Merge[T], I, Merge[U]] {
    type Delegates = transmittable.Type
  } =
    DelegatingTransmittable(
      provide = (value, context) => context.delegate(value.left -> value.right),
      receive = (value, context) => (Merge[U] _).tupled(context.delegate(value))
    )

  def isGlitched(v: Merge[Data[Merge[_]]]): Boolean = v.left.data != v.right.data

  class Host(name: String) extends FullMVEngine(10.second, name) {
    val registry   = new Registry
    def shutdown() = registry.terminate()

    import loci.serializer.jsoniterScala._
    import ReactiveTransmittable._
    import CodecHelper.given

    given fullData: JsonValueCodec[(String, ((String, Int), (String, Int)))] = JsonCodecMaker.make

    implicit val host: Host = this
    val derivedBinding      = Binding[Signal[Data[Merge[Data[Int]]]]]("derived")
  }

  class SideHost(name: String) extends Host(name) {
    val port: Int = TransmitterTestsPortManagement.getFreePort()
    registry.listen(TCP(port))

    import loci.serializer.jsoniterScala._
    import ReactiveTransmittable._
    import CodecHelper.given

    given intData: JsonValueCodec[(String, Int)] = JsonCodecMaker.make

    val sourceBinding = Binding[Signal[Data[Int]]]("source")

    val source       = Var(0)
    val taggedSource = source.map(Data(name, _))
    registry.bind(sourceBinding)(taggedSource)

    def step() = source.transform(_ + 1)
  }

  def run(runForMillis: Long): String = {
    val leftHost = new SideHost("left")
    try {
      val rightHost = new SideHost("right")
      try {
        val leftMerge = {
          import leftHost._
          val remoteRight     = Await.result(registry.connect(TCP("localhost", rightHost.port)), timeout)
          val sourceFromRight = Await.result(registry.lookup(sourceBinding, remoteRight), timeout)
          val merge           = Signal { Data("lmerge", Merge(taggedSource(), sourceFromRight())) }
          registry.bind(derivedBinding)(merge)
          merge
        }

        val rightMerge = {
          import rightHost._
          val remoteLeft     = Await.result(registry.connect(TCP("localhost", leftHost.port)), timeout)
          val sourceFromLeft = Await.result(registry.lookup(sourceBinding, remoteLeft), timeout)
          val merge          = Signal { Data("rmerge", Merge(sourceFromLeft(), taggedSource())) }
          registry.bind(derivedBinding)(merge)
          merge
        }

        val topHost = new Host("top")
        try {
          import topHost._

          val remoteLeft     = Await.result(registry.connect(TCP("localhost", leftHost.port)), timeout)
          val mergeFromLeft  = Await.result(registry.lookup(derivedBinding, remoteLeft), timeout)
          val remoteRight    = Await.result(registry.connect(TCP("localhost", rightHost.port)), timeout)
          val mergeFromRight = Await.result(registry.lookup(derivedBinding, remoteRight), timeout)
          val merge          = Signal { Merge(mergeFromLeft(), mergeFromRight()) }

          @volatile var violations: List[Merge[Data[Merge[Data[Int]]]]] = Nil
          merge.observe { v => if (isGlitched(v)) violations = v :: violations }

//          assert(violations.isEmpty)
//          assert(merge.readValueOnce === Merge(
//            Data("lmerge", Merge(Data("left", 0), Data("right", 0))),
//            Data("rmerge", Merge(Data("left", 0), Data("right", 0)))
//          ))
//
//          leftHost.step()
//
//          assert(violations.isEmpty)
//          assert(merge.readValueOnce === Merge(
//            Data("lmerge", Merge(Data("left", 1), Data("right", 0))),
//            Data("rmerge", Merge(Data("left", 1), Data("right", 0)))
//          ))
//
//          rightHost.step()
//
//          assert(violations.isEmpty)
//          assert(merge.readValueOnce === Merge(
//            Data("lmerge", Merge(Data("left", 1), Data("right", 1))),
//            Data("rmerge", Merge(Data("left", 1), Data("right", 1)))
//          ))

          println(
            s"starting X-Shape distributed Serializability stress test " + (if (runForMillis == 0) "until key press"
                                                                            else
                                                                              s"for ${runForMillis / 1000} seconds...")
          )
          @volatile var running: Boolean = true
          def worker(host: SideHost) =
            Spawn {
              try {
                var iterations = 0
                while (running) {
                  host.step()
                  iterations += 1
                  if (violations.nonEmpty) throw new RuntimeException(s"Found Violations after iteration $iterations")
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

          val workerTimeout = System.currentTimeMillis() + runForMillis
          while (
            running && (if (runForMillis == 0) System.in.available() == 0
                        else System.currentTimeMillis() < workerTimeout)
          ) {
            Thread.sleep(50)
          }
          if (!running) {
            println(
              s"Premature termination after ${(runForMillis - (workerTimeout - System.currentTimeMillis())) / 1000} seconds"
            )
          } else {
            println(s"Ran for ${(runForMillis - (workerTimeout - System.currentTimeMillis())) / 1000} seconds")
          }
          running = false

          val scoreLeft  = workerLeft.awaitTry(1000)
          val scoreRight = workerRight.awaitTry(1000)
          val scores     = Array(scoreLeft, scoreRight)
          println("X-Shape distributed Serializability stress test thread results:")
          println("\t" + scores.zipWithIndex.map { case (count, idx) => s"$idx: $count" }.mkString("\n\t"))
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
              println(s"sources: ${leftHost.source.readValueOnce} and ${rightHost.source.readValueOnce}")
              println(s"side merges: ${leftMerge.readValueOnce} and ${rightMerge.readValueOnce}")
              println(s"top merge: ${merge.readValueOnce}")
              "there were errors"
            case Some(sum) =>
              println(
                s"X-Shape distributed Serializability stress test totaled $sum iterations (individual scores: ${scores.mkString(", ")}"
              )
              if (violations.nonEmpty) {
                "There were violations:\r\n\t" + violations.mkString("\r\n\t")
              } else {
                val expected = Merge(
                  Data("lmerge", Merge(Data("left", scoreLeft.get), Data("right", scoreRight.get))),
                  Data("rmerge", Merge(Data("left", scoreLeft.get), Data("right", scoreRight.get)))
                )
                val actual = merge.readValueOnce
                if (actual != expected) {
                  s"Final value should be:\r\n\t$expected\r\nbut was\r\n\t$actual"
                } else {
                  val hosts = Array(leftHost, rightHost)
                  hosts.foreach(host => println(s"$host orphan stats: ${host.cacheStatus}"))

                  println(" == Orphan listing == ")
                  val ok = hosts.foldLeft(true) { (ok, host) =>
                    if (host.instances.isEmpty && host.lockHost.instances.isEmpty) {
                      ok
                    } else {
                      println(s"orphans on $host:")
                      val it1 = host.instances.values().iterator()
                      while (it1.hasNext) println("\t" + it1.next())
                      val it2 = host.lockHost.instances.values().iterator()
                      while (it2.hasNext) println("\t" + it2.next())
                      false
                    }
                  }
                  if (ok) null else "There were orphaned turn and/or lock instances."
                }
              }
          }
        } finally {
          topHost.shutdown()
        }
      } finally {
        rightHost.shutdown()
      }
    } finally {
      leftHost.shutdown()
    }
  }
}
