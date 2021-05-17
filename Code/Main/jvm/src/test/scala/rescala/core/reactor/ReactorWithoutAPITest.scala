package rescala.core.reactor

import rescala.core.{CreationTicket, Interp, MacroAccess, ReName}
import rescala.default
import tests.rescala.testtools.RETests

class ReactorWithoutAPITest extends RETests {

  import rescala.default._

  class CustomReactorReactive[T](
      initState: ReStructure#State[ReactorStage[T], ReStructure]
  ) extends Derived
      with Interp[T, ReStructure]
  with MacroAccess[T, Interp[T, ReStructure]]{

    override type Value = ReactorStage[T]

    override protected[rescala] def state: State = initState
    override protected[rescala] def name: ReName = "Custom Reactor"

    /** Interprets the internal type to the external type
      *
      * @group internal
      */
    override def interpret(v: ReactorStage[T]): T = v.value

    /** called if any of the dependencies ([[ReSource]]s) changed in the current update turn,
      * after all (known) dependencies are updated
      */
    override protected[rescala] def reevaluate(input: ReIn): Rout = {
      input.withValue(state.current.eval(input))
    }

    /** Defines how the state is modified on commit.
      *
      * Reactors don't change their state at the end of a transaction.
      *
      * @param base the reactor's state at the end of the transaction.
      * @return the reactor's state after the transaction is finished.
      */
    override protected[rescala] def commit(base: ReactorStage[T]): ReactorStage[T] = base

    override def resource: Interp[T, default.ReStructure] = this
  }

  /** A class that manages a single stage of the reactor body.
    *
    * A stage is a body of code, that gets executed in the same transaction.
    *
    * @param initialValue the value the reactor is initialized with.
    * @param reactor the reactor housing the stage.
    * @tparam T the value type of the reactor.
    */
  class ReactorStage[T](initialValue: T, reactor: CustomReactorReactive[T]) {

    /** The value of the stage.
      *
      * Because the value of the reactor equals to the value of its current
      * ReactorStage this is subsequently also the value of the reactor.
      */
    var value: T                                                     = initialValue

    /** A function that returns the reactor stage resulting of a reevaluation. */
    private var stageProgressor: Option[reactor.ReIn => ReactorStage[T]] = None

    /** Reevaluates the stage, if it can progress.
      *
      * @param input a [[rescala.core.ReevTicket]] of the reactor.
      * @return the progressed [[ReactorStage]].
      *         If the next stage isn't triggered yet, it returns the
      *         current stage.
      */
    def eval(input: reactor.ReIn): ReactorStage[T] = {
      stageProgressor.foreach { progressor =>
        return progressor(input)
      }

      this
    }

    def set(newValue: T): Unit = {
      value = newValue
    }

    /** Waits until the event is triggered.
      *
      * When the event is triggered the given body is executed in the
      * same transaction.
      *
      * @param event the event to wait for.
      * @param body the code to execute when the event is triggered.
      * @tparam E the event's type.
      */
    def next[E](event: Evt[E])(body: (ReactorStage[T], E) => Unit): Unit = {
      stageProgressor = Some(stageProgressor(event, body)(_: reactor.ReIn))
    }

    /** Encapsulates a stage that is waiting for progression.
      *
      * The function can be used to hide the event's type from the
      * external API.
      *
      * @param event the event that triggers the next stage.
      * @param body the body of the next stage.
      * @param input a [[rescala.core.ReevTicket]] of the reactor.
      * @tparam E the event's type.
      * @return the next stage, if the event is triggered.
      *         Otherwise returns the current stage.
      */
    private def stageProgressor[E](
        event: Evt[E],
        body: (ReactorStage[T], E) => Unit
    )(input: reactor.ReIn): ReactorStage[T] = {
      val eventValue = input.dependStatic(event)

      eventValue.foreach { eventValue =>
        val nextStage = new ReactorStage[T](this.value, reactor)
        body(nextStage, eventValue)

        return nextStage
      }

      this
    }
  }

  /** A ReactorStage that can be used for stage construction.
    *
    * This special stage is stripped off of all capabilities that
    * aren't known during initialization. Especially it doesn't need
    * to have a reactor.
    *
    * Therefore this stage is highly limited in its capabilities.
    * Its only purpose is to be upgraded into a real stage later.
    *
    * @param initialValue the value the reactor is initialized with.
    * @tparam T the value type of the reactor.
    */
  class ConstructionStage[T](initialValue: T) extends ReactorStage(initialValue, null) {

    // TODO: Throw errors if other methods are called. Probably UnsupportedOperationExceptions

    /** Constructs a real ReactorStage.
      *
      * This function merges the state of this ConstructionStage
      * with the given reactor and returns a real ReactorStage,
      * holding the state of the current stage.
      *
      * @param reactor the reactor holding the stage.
      * @return the resulting [[ReactorStage]].
      */
    def upgrade(reactor: CustomReactorReactive[T]): ReactorStage[T] = {
      new ReactorStage[T](value, reactor)
    }
  }

  object CustomReactorReactive {

    /** Returns a reactor that is executed once.
      *
      * @param initialValue the value the reactor has after initializaiton.
      * @param dependencies all events the reactor stages depend on.
      * @param body the reactor's body.
      * @tparam T the reactor's value type.
      * @return the resulting reactor object.
      */
    def once[T](initialValue: T, dependencies: Set[ReSource])(body: ReactorStage[T] => Unit): CustomReactorReactive[T] = {
      val initialStage = new ConstructionStage[T](initialValue)
      val reactor: CustomReactorReactive[T] = CreationTicket.fromScheduler(scheduler)
        .create(
          dependencies,
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

  test("Reactor has no glitches") {
    val e1 = Evt[String]()

    val reactor: CustomReactorReactive[String] = CustomReactorReactive.once("Initial Value", Set(e1)) { self =>
      self.set("Not Reacted")
      self.next(e1) { (self, _) =>
        self.set("Reacted")
      }
    }

    val tuple = Signal { (e1.latest("Init").value, reactor.value)}
    val history = tuple.changed.last(5)

    assert(tuple.now === (("Init", "Not Reacted")))
    assert(history.now === Nil)

    e1.fire("Fire")

    assert(tuple.now === (("Fire", "Reacted")))
    assert(history.now === List(("Fire", "Reacted")))
  }
}
