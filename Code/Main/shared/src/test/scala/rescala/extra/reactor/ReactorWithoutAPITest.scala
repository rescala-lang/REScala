package rescala.extra.reactor

import _root_.tests.rescala.testtools.RETests

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
        Stage().set("Value Set!")
      }

      assert(transaction(reactor) { _.now(reactor) } === "Value Set!")
    }

    test("Reactor waits for event when using next") {
      val e1 = Evt[Unit]()
      val reactor = Reactor.once(42) {
        Stage().next(e1) {
          Stage().set(1)
        }
      }

      assert(transaction(reactor) { _.now(reactor) } === 42)
      e1.fire()
      assert(transaction(reactor) { _.now(reactor) } === 1)
    }

    test("ReactorStage callback passes event value") {
      val e1 = Evt[Int]()

      val reactor = Reactor.once(0) {
        Stage().next(e1) { (e) =>
          Stage().set(e)
        }
      }

      assert(transaction(reactor) { _.now(reactor) } === 0)
      e1.fire(1)
      assert(transaction(reactor) { _.now(reactor) } === 1)
    }

    test("ReactorStages can be nested") {
      val e1 = Evt[Unit]()

      val reactor = Reactor.once(0) {
        Stage().next(e1) {
          Stage().set(1)
            .next(e1) {
              Stage().set(2)
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
        Stage().set("Not Reacted")
          .next(e1) { _ =>
            Stage().set("Reacted")
          }
      }

      val tuple   = Signal { (e1.latest("Init").value, reactor.value) }
      val history = tuple.changed.last(5)

      assert(tuple.now === (("Init", "Not Reacted")))
      assert(history.now === Nil)

      e1.fire("Fire")

      assert(tuple.now === (("Fire", "Reacted")))
      assert(history.now === List(("Fire", "Reacted")))
    }

    test("Rector can loop") {
      val e1 = Evt[Unit]()
      val reactor = Reactor.loop("Initial Value") {
        Stage().set("First Stage").next(e1) {
          Stage().set("Second Stage").next(e1) { Stage() }
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
        Stage().read(currentValue =>
          Stage().set(currentValue + 8)
        )
      }

      assert(reactor.now === 50)
    }

    test("Reactor read can branch") {
      val e1 = Evt[Int]()
      val reactor = Reactor.loop("") {
        Stage().next(e1) { eventValue =>
          Stage().read(_ =>
            if (eventValue > 10) {
              Stage().set("Greater 10")
            } else {
              Stage().set("Smaller 10")
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
        Stage().loop {
          Stage().next(e1) { eventValue =>
            Stage[Int]().modify(currentValue => currentValue + eventValue)
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
          Stage().next(e1) { eventValue =>
            Stage[Int]()
              .modify(currentValue => currentValue + eventValue)
              .next(e1) { eventValue =>
                Stage().modify(currentValue => currentValue - eventValue)
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
        Stage().until(e1,
          body = {
            Stage().set("Body value")
          },
          interruptHandler = {
            Stage().set("Interrupt value")
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
        Stage().until(e1,
          body = {
            Stage().set("Body value")
          },
          interruptHandler = { (e: String) =>
            Stage().set(e)
          }
        )
      }

      assert(reactor.now === "Body value")
      e1.fire("Event value")
      assert(reactor.now === "Event value")
    }

    test("Reactor until can contain loops") {
      val interrupt = Evt[Unit]()
      val modifyEvent = Evt[String]()
      val reactor = Reactor.once("Initial Value") {
        Stage().until(interrupt,
          body = {
            Stage().loop {
              Stage().next(modifyEvent) { eventValue =>
                Stage().set(eventValue)
              }
            }
          },
          interruptHandler = {
            Stage().set("Interrupted")
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
      val modifier = Evt[Int]()
      val reactor = Reactor.once(0) {
        Stage().until(interrupt,
          body = {
            Stage().loop {
              Stage().next(modifier) { v =>
                Stage().set(v)
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
  }
}
