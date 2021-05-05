package rescala.core.reactor

import tests.rescala.testtools.RETests

class ReactorWithoutAPITest extends RETests {

  import rescala.default._

  object CustomReactor {
    def once(body: ReactorStage => Unit): Unit = {
      body(new ReactorStage)
    }
  }

  class ReactorStage {
    def next[A](event: Evt[A])(callback: A => Unit): Unit = {
      event.observe({ x =>
        callback(x)
      })
    }
  }

  test("Reactor waits for event when using next") {
    val state = Var(0)
    val e1 = Evt[Unit]()

    assert(state.now === 0)

    CustomReactor.once { self =>
      state.set(1)
      self.next(e1) { res =>
        state.set(2)
      }
    }

    assert(state.now == 1)
    e1.fire()
    assert(state.now == 2)
  }
}
