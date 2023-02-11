package tests.rescala.fullmv.transmitter

import org.scalatest.funsuite.AnyFunSuite
import rescala.fullmv.DistributedFullMVApi.{Event, Evt, FullMVEngine, ReactiveTransmittable, Signal, Var}
import loci.communicator.tcp.TCP
import loci.registry.{Binding, Registry}

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Await
import scala.concurrent.duration._

import com.github.plokhotnyuk.jsoniter_scala.macros._
import com.github.plokhotnyuk.jsoniter_scala.core._

class ReactiveTransmittableTest extends AnyFunSuite {

  class Host(name: String) extends FullMVEngine(10.second, name) {
    val registry = new Registry

    implicit val host: Host = this

    import loci.serializer.jsoniterScala._
    import ReactiveTransmittable._
    import CodecHelper.given

    given JsonValueCodec[Int] = JsonCodecMaker.make

    val binding = Binding[Signal[Int]]("signal")
  }

  test("basic transmission works") {
    val hostA = new Host("basicA")
    val port  = TransmitterTestsPortManagement.getFreePort()
    hostA.registry.listen(TCP(port))
    try {
      val input = { import hostA._; Var(5) }
      hostA.registry.bind(hostA.binding)(input)

      val hostB   = new Host("basicB")
      val remoteA = Await.result(hostB.registry.connect(TCP("localhost", port)), 10.second)
      val reflection: Signal[Int] =
        Await.result(hostB.registry.lookup(hostB.binding, remoteA), 10.second)
      Thread.sleep(1000)

      assert(reflection.readValueOnce === 5)

      { import hostA._; input.set(123) }

      assert(reflection.readValueOnce === 123)
    } finally {
      hostA.registry.terminate()
    }
  }

  test("transmission supports derivations") {
    val hostA = new Host("derivationA")
    val port  = TransmitterTestsPortManagement.getFreePort()
    hostA.registry.listen(TCP(port))
    try {
      val input = { import hostA._; Var(5) }
      hostA.registry.bind(hostA.binding)(input)

      val hostB   = new Host("derivationB")
      val remoteA = Await.result(hostB.registry.connect(TCP("localhost", port)), 10.second)
      val reflection: Signal[Int] =
        Await.result(hostB.registry.lookup(hostB.binding, remoteA), 10.second)

      val derived = { import hostB._; reflection.map(_ * 2) }

      Thread.sleep(1000)

      assert(derived.readValueOnce === 10)

      { import hostA._; input.set(123) }

      assert(derived.readValueOnce === 246)
    } finally {
      hostA.registry.terminate()
    }
  }

  test("transmission maintains glitch freedom") {
    class GFHost(name: String) extends Host(name) {

      import loci.serializer.jsoniterScala._
      import ReactiveTransmittable._

      import CodecHelper.given

      given JsonValueCodec[(String, Int)] = JsonCodecMaker.make

      val branch1 = Binding[Signal[(String, Int)]]("branch1")
      val branch2 = Binding[Signal[(String, Int)]]("branch2")
    }

    val hostA = new GFHost("gfA")
    val port  = TransmitterTestsPortManagement.getFreePort()
    hostA.registry.listen(TCP(port))
    try {
      val input    = { import hostA._; Var(5) }
      val branch1A = { import hostA._; input.map("1a" -> _) }
      hostA.registry.bind(hostA.branch1)(branch1A)
      hostA.registry.bind(hostA.binding)(input)
      val branch2A = { import hostA._; input.map("2a" -> _) }
      hostA.registry.bind(hostA.branch2)(branch2A)

      val hostB   = new GFHost("gfB")
      val remoteA = Await.result(hostB.registry.connect(TCP("localhost", port)), 10.second)

      val remoteBranch1: Signal[(String, Int)] =
        Await.result(hostB.registry.lookup(hostB.branch1, remoteA), 10.second)
      val reflection: Signal[Int] =
        Await.result(hostB.registry.lookup(hostB.binding, remoteA), 10.second)
      val remoteBranch2: Signal[(String, Int)] =
        Await.result(hostB.registry.lookup(hostB.branch2, remoteA), 10.second)

      val branch1B = { import hostB._; remoteBranch1.map("1b" -> _) }
      val branch2B = { import hostB._; remoteBranch2.map("2b" -> _) }

      val tracker = ArrayBuffer[((String, (String, Int)), Int, (String, (String, Int)))]()
      val derived = {
        import hostB._;
        Signal {
          tracker.synchronized {
            val v = (branch1B(), reflection(), branch2B())
            tracker += v
            v
          }
        }
      }

      Thread.sleep(1000)

      assert(derived.readValueOnce === ((("1b", ("1a", 5)), 5, ("2b", ("2a", 5)))))
      assert(tracker === ArrayBuffer((("1b", ("1a", 5)), 5, ("2b", ("2a", 5)))))
      tracker.clear()

      { import hostA._; input.set(123) }

      assert(derived.readValueOnce === ((("1b", ("1a", 123)), 123, ("2b", ("2a", 123)))))
      assert(tracker === ArrayBuffer((("1b", ("1a", 123)), 123, ("2b", ("2a", 123)))))
    } finally {
      hostA.registry.terminate()
    }
  }

  test("events work too") {
    class GFHost(name: String) extends Host(name) {
      import ReactiveTransmittable._

      import loci.serializer.jsoniterScala._
      import ReactiveTransmittable._
      import CodecHelper.given

      given JsonValueCodec[(String, Int)] = JsonCodecMaker.make

      val branch1  = Binding[Event[(String, Int)]]("branch1")
      val branch2  = Binding[Event[(String, Int)]]("branch2")
      val eBinding = Binding[Event[Int]]("event")
    }

    val hostA = new GFHost("gfA")
    val port  = TransmitterTestsPortManagement.getFreePort()
    hostA.registry.listen(TCP(port))
    try {
      val input    = { import hostA._; Evt[Int]() }
      val branch1A = { import hostA._; input.map("1a" -> _) }
      hostA.registry.bind(hostA.branch1)(branch1A)
      hostA.registry.bind(hostA.eBinding)(input)
      val branch2A = { import hostA._; input.map("2a" -> _) }
      hostA.registry.bind(hostA.branch2)(branch2A)

      val hostB   = new GFHost("gfB")
      val remoteA = Await.result(hostB.registry.connect(TCP("localhost", port)), 10.second)

      val remoteBranch1: Event[(String, Int)] =
        Await.result(hostB.registry.lookup(hostB.branch1, remoteA), 10.second)
      val reflection: Event[Int] =
        Await.result(hostB.registry.lookup(hostB.eBinding, remoteA), 10.second)
      val remoteBranch2: Event[(String, Int)] =
        Await.result(hostB.registry.lookup(hostB.branch2, remoteA), 10.second)

      val branch1B = { import hostB._; remoteBranch1.map("1b" -> _) }
      val branch2B = { import hostB._; remoteBranch2.map("2b" -> _) }

      val tracker = ArrayBuffer[(Option[(String, (String, Int))], Option[Int], Option[(String, (String, Int))])]()
      val derived = {
        import hostB._;
        Event {
          tracker.synchronized {
            val v = (branch1B(), reflection(), branch2B())
            tracker += v
            Some(v)
          }
        }
      }
      val hold = { import hostB._; derived.last(1) }

      Thread.sleep(1000)

      assert(hold.readValueOnce === List())
      assert(tracker.isEmpty) // in case this failed, someone may have changed event initialization semantics again. Try instead for === ArrayBuffer((None, None, None))
      tracker.clear()

      { import hostA._; input.fire(123) }

      assert(hold.readValueOnce === List((
        Some(("1b", ("1a", 123))),
        Some(123),
        Some(("2b", ("2a", 123)))
      )))
      assert(tracker === ArrayBuffer((Some(("1b", ("1a", 123))), Some(123), Some(("2b", ("2a", 123))))))
    } finally {
      hostA.registry.terminate()
    }
  }
}
