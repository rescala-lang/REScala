package rescala.extra.reactor

import _root_.tests.rescala.testtools.RETests

class ReactorWithoutAPITest extends RETests {

  multiEngined { engine =>
    val reactorApi = new ReactorBundle[engine.type](engine)
    import engine._
    import reactorApi._

    test("Reactor has initial value") {
      val reactor = Reactor.once("Initial Value") { StageBuilder() }

      assert(transaction(reactor) { _.now(reactor) } === "Initial Value")
    }

    test("Reactor executes body instantly") {
      val reactor = Reactor.once("Initial Value") {
        StageBuilder().set("Value Set!")
      }

      assert(transaction(reactor) { _.now(reactor) } === "Value Set!")
    }

    test("Reactor waits for event when using next") {
      val e1 = Evt[Unit]()
      val reactor = Reactor.once(42) {
        StageBuilder().next(e1) { _ =>
          StageBuilder().set(1)
        }
      }

      assert(transaction(reactor) { _.now(reactor) } === 42)
      e1.fire()
      assert(transaction(reactor) { _.now(reactor) } === 1)
    }

    test("ReactorStage callback passes event value") {
      val e1 = Evt[Int]()

      val reactor = Reactor.once(0) {
        StageBuilder().next(e1) { (e) =>
          StageBuilder().set(e)
        }
      }

      assert(transaction(reactor) { _.now(reactor) } === 0)
      e1.fire(1)
      assert(transaction(reactor) { _.now(reactor) } === 1)
    }

    test("ReactorStages can be nested") {
      val e1 = Evt[Unit]()

      val reactor = Reactor.once(0) {
        StageBuilder().next(e1) { (_) =>
          StageBuilder().set(1)
            .next(e1) { (_) =>
              StageBuilder().set(2)
            }
        }
      }

      assert(reactor.now === 0)
      e1.fire()
      assert(reactor.now === 1)
      e1.fire()
      assert(reactor.now === 2)
    }

    "Reactor has no glitches" ignore {
      val e1 = Evt[String]()

      val reactor = Reactor.once("Initial Value") {
        StageBuilder().set("Not Reacted")
          .next(e1) { _ =>
            StageBuilder().set("Reacted")
          }
      }

      val tuple   = Signal { (e1.latest("Init").value, reactor.now) }
      val history = tuple.changed.last(5)

      assert(tuple.now === (("Init", "Not Reacted")))
      assert(history.now === Nil)

      e1.fire("Fire")

      assert(tuple.now === (("Fire", "Reacted")))
      assert(history.now === List(("Fire", "Reacted")))
    }

    test("Recrot can loop") {
      val e1 = Evt[Unit]()
      val reactor = Reactor.loop("Initial Value") {
        StageBuilder().set("First Stage").next(e1) { _ =>
          {
            StageBuilder().set("Second Stage").next(e1) { _ => StageBuilder() }
          }
        }
      }

      assert(reactor.now === "First Stage")
      e1.fire()
      assert(reactor.now === "Second Stage")
      e1.fire()
      assert(reactor.now === "First Stage")
      e1.fire()
      assert(reactor.now === "Second Stage")
    }
  }
}
