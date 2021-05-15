package rescala.core.reactor

import rescala.core.{CreationTicket, Interp, ReName, Scheduler}
import tests.rescala.testtools.RETests

class ReactorWithoutAPITest extends RETests {

  import rescala.default._

  class CustomReactorReactive[T](initState: ReStructure#State[ReactorStage[T], ReStructure])
      extends Derived
      with Interp[T, ReStructure] {
    override type Value = ReactorStage[T]

    /** Interprets the internal type to the external type
      *
      * @group internal
      */
    override def interpret(v: Value): T = v.run()

    override protected[rescala] def name: ReName = "Custom Reactor"

    /** Return the value of the Reactor after the end of a transaction.
      *
      * This method is called at the end of transactions.
      * The value of [[CustomReactorReactive]] does not change
      * at the end of a transaction.
      *
      * @param base the reactor's current value
      * @return the value provided by base
      */
    override protected[rescala] def commit(base: ReactorStage[T]): ReactorStage[T] = base

    /** called if any of the dependencies ([[ReSource]]s) changed in the current update turn,
      * after all (known) dependencies are updated
      */
    override protected[rescala] def reevaluate(input: ReIn): Rout = {
      state.current.run()

      if (state.current.nextStage.isEmpty) {
        return input
      }

      val nextStage = state.current.nextStage.get

      // Should never be None. Every stage except the
      // initial Stage has a trigger.
      val trigger   = nextStage.trigger.get

      val triggered = input.dependStatic(trigger)
      if (triggered.isEmpty) {
        return input
      }

      input.withValue(nextStage)
    }

    override protected[rescala] def state: State = initState
  }

  class ReactorStage[T](var trigger: Option[Evt[Unit]], initialValue: T)(body: ReactorStage[T] => Unit) {
    private var value: T                                    = initialValue
    private[reactor] var nextStage: Option[ReactorStage[T]] = None

    def next(event: Evt[Unit])(callback: ReactorStage[T] => Unit): Unit = {
      nextStage = Some(new ReactorStage[T](Some(event), value)(callback))
    }

    def set(newValue: T): Unit = {
      value = newValue
    }

    private[reactor] def run(): T = {
      body(this)
      value
    }
  }

  object CustomReactorReactive {
    def once[T](initialValue: T, inputs: Set[ReSource])(body: ReactorStage[T] => Unit)(implicit
        fac: Scheduler[ReStructure]
    ): CustomReactorReactive[T] = {
      val customReactor =
        CreationTicket.fromScheduler(scheduler)
          .create(
            inputs,
            new ReactorStage[T](None, initialValue)(body),
            inite = true
          ) { createdState =>
            new CustomReactorReactive[T](createdState)
          }

      customReactor
    }
  }

  test("Reactor has value") {
    val reactor = CustomReactorReactive.once("Initial Value", Set()) { self =>
      self.set("Value Set!")
    }

    assert(transaction(reactor) { _.now(reactor) } === "Value Set!")
  }

  test("Reactor waits for event when using next") {
    val e1 = Evt[Unit]()
    val reactor = CustomReactorReactive.once(42, Set(e1)) { self =>
      self.next(e1) { self =>
        self.set(1)
      }
    }

    assert(transaction(reactor) { _.now(reactor) } === 42)
    e1.fire()
    assert(transaction(reactor) { _.now(reactor) } === 1)
  }
}
