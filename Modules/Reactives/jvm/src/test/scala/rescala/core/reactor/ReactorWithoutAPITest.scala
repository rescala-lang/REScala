package rescala.core.reactor

import rescala.core.{CreationTicket, ReInfo, ReSource, ReadAs}
import rescala.macros.MacroAccess
import tests.rescala.testtools.RETests

class ReactorWithoutAPITest extends RETests {

  import ReactorAction._
  import rescala.default._

  class Reactor[T](
      initState: State[ReactorStage[T]]
  ) extends rescala.core.Derived
      with ReadAs[T]
      with MacroAccess[T] {

    override type Value    = ReactorStage[T]
    override type State[V] = rescala.default.State[V]

    override protected[rescala] def state: State[ReactorStage[T]] = initState
    override def info: ReInfo                                     = ReInfo.create.derive("Custom Reactor")

    /** Interprets the internal type to the external type
      *
      * @group internal
      */
    override def read(v: ReactorStage[T]): T = v.currentValue

    /** called if any of the dependencies ([[ReSource]]s) changed in the current update turn,
      * after all (known) dependencies are updated
      */
    override protected[rescala] def reevaluate(input: ReIn): Rout = {

      input.trackDependencies(Set())

      // todo, this should be tailrec, but then does not compile on 2.11
      // @tailrec
      def processActions[A](stage: ReactorStage[T]): ReactorStage[T] = {
        stage.stages.actions match {
          case Nil                  => stage
          case SetAction(v) :: tail => processActions(stage.copy(currentValue = v, stages = StageBuilder(tail)))
          case NextAction(event, handler) :: tail =>
            val eventValue = input.depend(event)
            eventValue match {
              case None => stage
              case Some(value) =>
                val stages = handler(value)
                processActions(stage.copy(stages = stages))
            }
        }
      }

      val resStage = processActions(input.before)

      input.withValue(resStage)
    }

    /** Defines how the state is modified on commit.
      *
      * Reactors don't change their state at the end of a transaction.
      *
      * @param base the reactor's state at the end of the transaction.
      * @return the reactor's state after the transaction is finished.
      */
    override protected[rescala] def commit(base: ReactorStage[T]): ReactorStage[T] = base

    def resource: ReadAs.of[State, T] = this

    def now: T = scheduler.forceNewTransaction(this)(at => at.now(this))
  }

  /** A class that manages a single stage of the reactor body.
    *
    * A stage is a body of code, that gets executed in the same transaction.
    *
    * @param currentValue the value the reactor is initialized with.
    * @param reactor the reactor housing the stage.
    * @tparam T the value type of the reactor.
    */
  case class ReactorStage[T](currentValue: T, stages: StageBuilder[T])

  sealed trait ReactorAction[T]
  object ReactorAction {
    case class SetAction[T](res: T)                                             extends ReactorAction[T]
    case class NextAction[E, T](event: Event[E], handler: E => StageBuilder[T]) extends ReactorAction[T]
  }

  case class StageBuilder[T](actions: List[ReactorAction[T]] = Nil) {
    import ReactorAction._

    private def addAction(newValue: ReactorAction[T]): StageBuilder[T] = {
      copy(actions = actions :+ newValue)
    }

    def set(newValue: T): StageBuilder[T] = {
      addAction(SetAction(newValue))
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
    def next[E](event: Evt[E])(body: (E => StageBuilder[T])): StageBuilder[T] = {
      addAction(NextAction(event, body))
    }

  }

  object Reactor {

    /** Returns a reactor that is executed once.
      *
      * @param initialValue the value the reactor has after initializaiton.
      * @param dependencies all events the reactor stages depend on.
      * @param body the reactor's body.
      * @tparam T the reactor's value type.
      * @return the resulting reactor object.
      */
    def once[T](
        initialValue: T,
        dependencies: Set[ReSource.of[State]]
    )(stageBuilder: StageBuilder[T])(implicit ct: CreationTicket[State]): Reactor[T] = {
      ct.create(
        dependencies,
        new ReactorStage[T](initialValue, stageBuilder),
        needsReevaluation = true
      ) { (createdState: State[ReactorStage[T]]) =>
        new Reactor[T](createdState)
      }
    }
  }

  test("Reactor has initial value") {
    val reactor = Reactor.once("Initial Value", Set()) { StageBuilder() }

    assert(transaction(reactor) { _.now(reactor) } === "Initial Value")
  }

  test("Reactor executes body instantly") {
    val reactor = Reactor.once("Initial Value", Set()) {
      StageBuilder().set("Value Set!")
    }

    assert(transaction(reactor) { _.now(reactor) } === "Value Set!")
  }

  test("Reactor waits for event when using next") {
    val e1 = Evt[Unit]()
    val reactor = Reactor.once(42, Set(e1)) {
      StageBuilder().next(e1) { (_) =>
        StageBuilder().set(1)
      }
    }

    assert(transaction(reactor) { _.now(reactor) } === 42)
    e1.fire()
    assert(transaction(reactor) { _.now(reactor) } === 1)
  }

  test("ReactorStage callback passes event value") {
    val e1 = Evt[Int]()

    val reactor = Reactor.once(0, Set(e1)) {
      StageBuilder().next(e1) { (e) =>
        StageBuilder().set(e)
      }
    }

    assert(transaction(reactor) { _.now(reactor) } === 0)
    e1.fire(1)
    assert(transaction(reactor) { _.now(reactor) } === 1)
  }

  "ReactorStages can be nested" ignore {
    val e1 = Evt[Unit]()

    val reactor = Reactor.once(0, Set(e1)) {
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

  test("Reactor has no glitches") {
    val e1 = Evt[String]()

    val reactor: Reactor[String] = Reactor.once("Initial Value", Set(e1)) {
      StageBuilder().set("Not Reacted")
        .next(e1) { (_) =>
          StageBuilder().set("Reacted")
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
}
