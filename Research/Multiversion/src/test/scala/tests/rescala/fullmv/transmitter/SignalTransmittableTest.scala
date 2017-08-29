package tests.rescala.fullmv.transmitter

import java.util.concurrent.atomic.AtomicInteger

import org.scalatest.FunSuite
import rescala.fullmv.transmitter.ReactiveTransmittable
import rescala.fullmv.{FullMVEngine, FullMVStruct}
import rescala.reactives.Signal
import retier.communicator.tcp.TCP
import retier.registry.{Binding, Registry}

import scala.concurrent.Await
import scala.concurrent.duration._

class SignalTransmittableTest extends FunSuite {
  val ports = new AtomicInteger(1099)
  class Host(name: String) extends FullMVEngine(10.second, name) {
    val registry = new Registry

    import io.circe.generic.auto._
    import rescala.fullmv.transmitter.CirceSerialization._
    import ReactiveTransmittable._
    implicit val host = this

    val binding = Binding[Signal[Int]]("signal")
  }

  test("basic transmission works") {
    val hostA = new Host("basicA")
    val port = ports.getAndIncrement()
    hostA.registry.listen(TCP(port))
    try {
      val input = {import hostA._; Var(5)}
      hostA.registry.bind(hostA.binding)(input)

      val hostB = new Host("basicB")
      val remoteA = Await.result(hostB.registry.request(TCP("localhost", port)), 10.second)
      val reflection: Signal[Int, FullMVStruct] = Await.result(hostB.registry.lookup(hostB.binding, remoteA), 10.second)
      Thread.sleep(1000)

      assert({import hostB._; reflection.now} === 5)
      ;{import hostA._; input.set(123)}
      assert({import hostB._; reflection.now} === 123)
    } finally {
      hostA.registry.terminate()
    }
  }

  test("transmission supports derivations") {
    val hostA = new Host("derivationA")
    val port = ports.getAndIncrement()
    hostA.registry.listen(TCP(port))
    try {
      val input = {import hostA._; Var(5)}
      hostA.registry.bind(hostA.binding)(input)

      val hostB = new Host("derivationB")
      val remoteA = Await.result(hostB.registry.request(TCP("localhost", port)), 10.second)
      val reflection: Signal[Int, FullMVStruct] = Await.result(hostB.registry.lookup(hostB.binding, remoteA), 10.second)
      Thread.sleep(1000)
      val derived = {import hostB._; reflection.map(_ * 2)}

      assert({import hostB._; derived.now} === 10)
      ;{import hostA._; input.set(123)}
      assert({import hostB._; derived.now} === 246)
    } finally {
      hostA.registry.terminate()
    }
  }

//  test("transmission maintains glitch freedom") {
//    val input = {import hostA._; Var(5)}
//    val branch1A = {import hostA._; input.map("1a" -> _)}
//    val branch1B = {import hostB._; ReactiveLocalClone(branch1A, hostB).map("1b" -> _)}
//    val reflection = ReactiveLocalClone(input, hostB)
//    val branch2A = {import hostA._; input.map("2a" -> _)}
//    val branch2B = {import hostB._; ReactiveLocalClone(branch2A, hostB).map("2b" -> _)}
//
//    val tracker = ArrayBuffer[((String, (String, Int)), Int, (String, (String, Int)))]()
//    val derived = {import hostB._; Signal {
//      tracker.synchronized {
//        val v = (branch1B(), reflection(), branch2B())
//        tracker += v
//        v
//      }
//    }}
//
//    assert({import hostB._; derived.now} === ((("1b", ("1a", 5)), 5, ("2b", ("2a", 5)))))
//    assert(tracker === ArrayBuffer((("1b", ("1a", 5)), 5, ("2b", ("2a", 5)))))
//    tracker.clear()
//
//    ;{import hostA._; input.set(123)}
//    assert({import hostB._; derived.now} === ((("1b", ("1a", 123)), 123, ("2b", ("2a", 123)))))
//    assert(tracker === ArrayBuffer((("1b", ("1a", 123)), 123, ("2b", ("2a", 123)))))
//  }
//
//  test("events work too") {
//    val input = {import hostA._; Evt[Int]()}
//    val branch1A = {import hostA._; input.map("1a" -> _)}
//    val branch1B = {import hostB._; ReactiveLocalClone(branch1A, hostB).map("1b" -> _)}
//    val reflection = ReactiveLocalClone(input, hostB)
//    val branch2A = {import hostA._; input.map("2a" -> _)}
//    val branch2B = {import hostB._; ReactiveLocalClone(branch2A, hostB).map("2b" -> _)}
//
//    val tracker = ArrayBuffer[(Option[(String, (String, Int))], Option[Int], Option[(String, (String, Int))])]()
//    val derived = {import hostB._; Event {
//      tracker.synchronized {
//        val v = (branch1B(), reflection(), branch2B())
//        tracker += v
//        Some(v)
//      }
//    }}
//    val hold = {import hostB._; derived.last(1)}
//
//    assert({import hostB._; hold.now} === List())
//    assert(tracker === ArrayBuffer((None, None, None))) // because dynamic events are stupid :) This *should* be tracker.isEmpty === true
//    tracker.clear()
//
//    ;{import hostA._; input.fire(123)}
//    assert({import hostB._; hold.now} === List((Some(("1b", ("1a", 123))), Some(123), Some(("2b", ("2a", 123))))))
//    assert(tracker === ArrayBuffer((Some(("1b", ("1a", 123))), Some(123), Some(("2b", ("2a", 123))))))
//  }
}
