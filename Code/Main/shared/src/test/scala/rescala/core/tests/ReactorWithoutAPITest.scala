package rescala.core.tests

import rescala.operator.Observe.ObserveInteract
import rescala.operator.RExceptions.ObservedException
import rescala.operator.{Event, Observe, Pulse}
import tests.rescala.testtools.RETests

class ReactorWithoutAPITest extends RETests {
  multiEngined { engine =>
    import engine._

    object CustomReactor {
      def once(body: CustomReactor=>Unit): CustomReactor = {
        val reactor = new CustomReactor(body)
        reactor.execOnce()
        reactor
      }
    }

    class CustomReactor(body: CustomReactor=>Unit) {

      private def execOnce(): Unit = {
        // Execute on new thread, which can be suspended in case of next()?
        body(this)
      }

      def next[A](event: Evt[A]): Unit = {
        Observe.strong(event, fireImmediately = false) { evtVal =>
          val internalVal = event.internalAccess(evtVal)
          new ObserveInteract {
            override def checkExceptionAndRemoval(): Boolean = {
              evtVal match {
                case Pulse.Exceptional(f) =>
                  throw ObservedException(event, "observed", f)
                case _ => ()
              }
              false
            }

            override def execute(): Unit =
              internalVal match {
                case Pulse.NoChange       => ()
                case Pulse.Value(v)       => println("Continue body execution") // Continue body thread?
                case Pulse.Exceptional(f) => ()  // Error handling?
              }
          }
        }

      }
    }

    test("Reactor waits for event when using next") {
      val state = Var(0)
      val e1 = Evt[Unit]()

      assert(state.now === 0)

      CustomReactor.once { self =>
        state.set(1)
        self next e1
        state.set(2)
      }

      assert(state.now == 1)
      e1.fire()
      assert(state.now == 2)
    }
  }
}
