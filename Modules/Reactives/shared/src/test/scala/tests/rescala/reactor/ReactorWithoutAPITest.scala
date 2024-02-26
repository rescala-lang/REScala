package tests.rescala.reactor

import _root_.tests.rescala.testtools.RETests
import reactives.extra.reactor.ReactorBundle

class ReactorWithoutAPITest extends RETests {

  multiEngined { engine =>
    val reactorApi = new ReactorBundle[engine.type](engine)
    import engine._
    import reactorApi._

    test("Reactor has initial value") {
      val reactor = Reactor.once("Initial Value") { Stage() }

      assert(transaction(reactor) { _.now(reactor) } === "Initial Value")
    }

    test("Reactor executes body instantly") {
      val reactor = Reactor.once("Initial Value") {
        S.set("Value Set!")
      }

      assert(transaction(reactor) { _.now(reactor) } === "Value Set!")
    }

    test("Reactor waits for event when using next") {
      val e1 = Evt[Unit]()
      val reactor = Reactor.once(42) {
        S.next(e1) {
          S.set(1)
        }
      }

      assert(transaction(reactor) { _.now(reactor) } === 42)
      e1.fire()
      assert(transaction(reactor) { _.now(reactor) } === 1)
    }

    test("ReactorStage callback passes event value") {
      val e1 = Evt[Int]()

      val reactor = Reactor.once(0) {
        S.next(e1) { e =>
          S.set(e)
        }
      }

      assert(transaction(reactor) { _.now(reactor) } === 0)
      e1.fire(1)
      assert(transaction(reactor) { _.now(reactor) } === 1)
    }

    test("ReactorStages can be nested") {
      val e1 = Evt[Unit]()

      val reactor = Reactor.once(0) {
        S.next(e1) {
          S.set(1)
            .next(e1) {
              S.set(2)
            }
        }
      }

      assert(reactor.now === 0)
      e1.fire()
      assert(reactor.now === 1)
      e1.fire()
      assert(reactor.now === 2)
    }

    test("Reactor has no glitches") {
      val e1 = Evt[String]()

      val reactor = Reactor.once("Initial Value") {
        S.set("Not Reacted")
          .next(e1) { _ =>
            S.set("Reacted")
          }
      }

      val tuple   = Signal { (e1.hold("Init").value, reactor.value) }
      val history = tuple.changed.list(5)

      assert(tuple.now === (("Init", "Not Reacted")))
      assert(history.now === Nil)

      e1.fire("Fire")

      assert(tuple.now === (("Fire", "Reacted")))
      assert(history.now === List(("Fire", "Reacted")))
    }

    test("Rector can loop") {
      val e1 = Evt[Unit]()
      val reactor = Reactor.loop("Initial Value") {
        S.set("First Stage").next(e1) {
          S.set("Second Stage").next(e1) { Stage() }
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

    test("Reactor read") {
      val reactor = Reactor.once(42) {
        S.read(currentValue =>
          S.set(currentValue + 8)
        )
      }

      assert(reactor.now === 50)
    }

    test("Reactor read can branch") {
      val e1 = Evt[Int]()
      val reactor = Reactor.loop("") {
        S.next(e1) { eventValue =>
          S.read(_ =>
            if (eventValue > 10) {
              S.set("Greater 10")
            } else {
              S.set("Smaller 10")
            }
          )
        }
      }

      assert(reactor.now === "")
      e1.fire(42)
      assert(reactor.now === "Greater 10")
      e1.fire(9)
      assert(reactor.now === "Smaller 10")
    }

    test("Reactor stage loop") {
      val e1 = Evt[Int]()
      val reactor = Reactor.once(42) {
        S.loop {
          S.next(e1) { eventValue =>
            S.modify(currentValue => currentValue + eventValue)
          }
        }
      }

      assert(reactor.now === 42)
      e1.fire(1)
      assert(reactor.now === 43)
      e1.fire(7)
      assert(reactor.now === 50)
    }

    test("Reactor complex stage loop") {
      val e1 = Evt[Int]()
      val reactor = Reactor.once(42) {
        Stage().loop {
          S.next(e1) { eventValue =>
            S.modify((currentValue: Int) => currentValue + eventValue)
              .next(e1) { eventValue =>
                S.modify(currentValue => currentValue - eventValue)
              }
          }
        }
      }

      assert(reactor.now === 42)
      e1.fire(1)
      assert(reactor.now === 43)
      e1.fire(3)
      assert(reactor.now === 40)
      e1.fire(10)
      assert(reactor.now === 50)
      e1.fire(50)
      assert(reactor.now === 0)
    }

    test("Reactor until works") {
      val e1 = Evt[Unit]()
      val reactor = Reactor.once("Initial Value") {
        S.until(
          e1,
          body = {
            S.set("Body value")
          },
          interruptHandler = {
            S.set("Interrupt value")
          }
        )
      }

      assert(reactor.now === "Body value")
      e1.fire()
      assert(reactor.now === "Interrupt value")
    }

    test("Reactor until passes event value") {
      val e1 = Evt[String]()
      val reactor = Reactor.once("Initial Value") {
        S.until(
          e1,
          body = {
            S.set("Body value")
          },
          interruptHandler = { (e: String) =>
            S.set(e)
          }
        )
      }

      assert(reactor.now === "Body value")
      e1.fire("Event value")
      assert(reactor.now === "Event value")
    }

    test("Reactor until can contain loops") {
      val interrupt   = Evt[Unit]()
      val modifyEvent = Evt[String]()
      val reactor = Reactor.once("Initial Value") {
        S.until(
          interrupt,
          body = {
            S.loop {
              S.next(modifyEvent) { eventValue =>
                S.set(eventValue)
              }
            }
          },
          interruptHandler = {
            S.set("Interrupted")
          }
        )
      }

      assert(reactor.now === "Initial Value")
      modifyEvent.fire("First Value")
      assert(reactor.now === "First Value")
      modifyEvent.fire("Second Value")
      assert(reactor.now === "Second Value")
      interrupt.fire()
      assert(reactor.now === "Interrupted")
    }

    test("Reactor until works without interruptHandler") {
      val interrupt = Evt[Unit]()
      val modifier  = Evt[Int]()
      val reactor = Reactor.once(0) {
        S.until(
          interrupt,
          body = {
            S.loop {
              S.next(modifier) { v =>
                S.set(v)
              }
            }
          }
        )
      }

      assert(reactor.now === 0)
      modifier.fire(42)
      assert(reactor.now === 42)
      interrupt.fire()
      modifier.fire(50)
      assert(reactor.now === 42)
    }

    test("Reactor multiple changes in a single stage") {
      val start = Evt[Unit]()
      val reactor = Reactor.once(0) {
        S.next(start) {
          S.set(1).set(42)
        }
      }

      val reactorSignal  = Signal { reactor.value }
      val reactorChanged = reactorSignal.changed

      val counter = reactorChanged.count()
      start.fire()
      assert(counter.now === 1)
    }
  }
}
