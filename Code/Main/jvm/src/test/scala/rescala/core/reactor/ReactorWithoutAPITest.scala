package rescala.core.reactor

import rescala.core.{CreationTicket, Interp, ReName}
import tests.rescala.testtools.RETests

class ReactorWithoutAPITest extends RETests {

  import rescala.default._

  class CustomReactorReactive[T](
      initState: ReStructure#State[ReactorStage[T], ReStructure]
  ) extends Derived
      with Interp[T, ReStructure] {

    override type Value = ReactorStage[T]

    /** Interprets the internal type to the external type
      *
      * @group internal
      */
    override def interpret(v: ReactorStage[T]): T = v.value

    override protected[rescala] def name: ReName = "Custom Reactor"

    /** called if any of the dependencies ([[ReSource]]s) changed in the current update turn,
      * after all (known) dependencies are updated
      */
    override protected[rescala] def reevaluate(input: ReIn): Rout = {
      input.withValue(state.current.eval(input))
    }

    override protected[rescala] def state: State = initState

    override protected[rescala] def commit(base: ReactorStage[T]): ReactorStage[T] = base
  }

  class ReactorStage[T](initialValue: T, reactor: CustomReactorReactive[T]) {
    var value: T                                                     = initialValue
    private var stageHolder: Option[reactor.ReIn => ReactorStage[T]] = None

    def eval(input: reactor.ReIn): ReactorStage[T] = {
      stageHolder.foreach { holder =>
        return holder(input)
      }

      this
    }

    def set(newValue: T): Unit = {
      value = newValue
    }

    def next[E](event: Evt[E])(body: (ReactorStage[T], E) => Unit): Unit = {
      stageHolder = Some(stageHolder(event, body)(_: reactor.ReIn))
    }

    private def stageHolder[E](
        event: Evt[E],
        body: (ReactorStage[T], E) => Unit
    )(input: reactor.ReIn): ReactorStage[T] = {
      val eventDepend = input.dependStatic(event)

      if (eventDepend.isEmpty) {
        return this
      }

      val eventValue = eventDepend.get

      val nextStage = new ReactorStage[T](value, reactor)
      body(nextStage, eventValue)

      nextStage
    }
  }

  class BasicReactorStage[T](initialValue: T) extends ReactorStage(initialValue, null) {

    // TODO: Throw errors if other methods are called. Probably UnsupportedOperationExceptions

    def upgrade(reactor: CustomReactorReactive[T]): ReactorStage[T] = {
      new ReactorStage[T](value, reactor)
    }
  }

  object CustomReactorReactive {
    def once[T](initialValue: T, inputs: Set[ReSource])(body: ReactorStage[T] => Unit): CustomReactorReactive[T] = {
      val initialStage = new BasicReactorStage[T](initialValue)
      val reactor: CustomReactorReactive[T] = CreationTicket.fromScheduler(scheduler)
        .create(
          inputs,
          initialStage: ReactorStage[T],
          inite = false
        ) { createdState: ReStructure#State[ReactorStage[T], ReStructure] =>
          new CustomReactorReactive[T](createdState)
        }

      val reactorStage = initialStage.upgrade(reactor)
      body(reactorStage)
      reactor.state.current = reactorStage

      reactor
    }
  }

  test("Reactor has initial value") {
    val reactor = CustomReactorReactive.once("Initial Value", Set()) { _ => }

    assert(transaction(reactor) { _.now(reactor) } === "Initial Value")
  }

  test("Reactor executes body instantly") {
    val reactor = CustomReactorReactive.once("Initial Value", Set()) { self =>
      self.set("Value Set!")
    }

    assert(transaction(reactor) { _.now(reactor) } === "Value Set!")
  }

  test("Reactor waits for event when using next") {
    val e1 = Evt[Unit]()
    val reactor = CustomReactorReactive.once(42, Set(e1)) { self =>
      self.next(e1) { (self, _) =>
        self.set(1)
      }
    }

    assert(transaction(reactor) { _.now(reactor) } === 42)
    e1.fire()
    assert(transaction(reactor) { _.now(reactor) } === 1)
  }

  test("ReactorStage callback passes event value") {
    val e1 = Evt[Int]()

    val reactor = CustomReactorReactive.once(0, Set(e1)) { self =>
      self.next(e1) { (self, e) =>
        self.set(e)
      }
    }

    assert(transaction(reactor) { _.now(reactor) } === 0)
    e1.fire(1)
    assert(transaction(reactor) { _.now(reactor) } === 1)
  }

  test("ReactorStages can be nested") {
    val e1 = Evt[Unit]()

    val reactor = CustomReactorReactive.once(0, Set(e1)) { self =>
      self.next(e1) { (self, _) =>
        self.set(1)
        self.next(e1) { (self, _) =>
          self.set(2)
        }
      }
    }

    assert(transaction(reactor) { _.now(reactor) } === 0)
    e1.fire()
    assert(transaction(reactor) { _.now(reactor) } === 1)
    e1.fire()
    assert(transaction(reactor) { _.now(reactor) } === 2)
  }
}
