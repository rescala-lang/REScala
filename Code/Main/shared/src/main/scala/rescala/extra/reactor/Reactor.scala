package rescala.extra.reactor

import rescala.core.ReName
import rescala.interface.RescalaInterface
import rescala.macros.MacroAccess

import scala.annotation.tailrec

class ReactorBundle[Api <: RescalaInterface](val api: Api) {
  import api._
  class Reactor[T](
      initState: State[ReactorStage[T]],
      looping: Boolean = false,
  ) extends Derived with Interp[T] with MacroAccess[T, Interp[T]] {

    override type Value = ReactorStage[T]

    override protected[rescala] def state: State[ReactorStage[T]] = initState
    override protected[rescala] def name: ReName                  = "Custom Reactor"
    override def interpret(v: ReactorStage[T]): T                 = v.currentValue
    override protected[rescala] def commit(base: Value): Value    = base

    /** called if any of the dependencies changed in the current update turn,
      * after all (known) dependencies are updated
      */
    override protected[rescala] def reevaluate(input: ReIn): Rout = {

      input.trackDependencies(Set())

      var progressedStage = false

      @tailrec
      def processActions[A](stage: ReactorStage[T]): ReactorStage[T] = {
        stage.stages.actions match {
          case Nil => stage
          case ReactorAction.SetAction(v) :: tail =>
            processActions(stage.copy(currentValue = v, stages = StageBuilder(tail)))
          case ReactorAction.ModifyAction(modifier) :: tail =>
            val modifiedValue = modifier(stage.currentValue)
            processActions(stage.copy(currentValue = modifiedValue, stages = StageBuilder(tail)))
          case ReactorAction.NextAction(event, handler) :: _ =>
            if (progressedStage) {
              return stage
            }

            val eventValue = input.depend(event)
            eventValue match {
              case None => stage
              case Some(value) =>
                progressedStage = true
                val stages = handler(value)
                processActions(stage.copy(stages = stages))
            }
          case ReactorAction.ReadAction(builder) :: _ =>
            val nextStage = builder(stage.currentValue)
            processActions(stage.copy(stages = nextStage))
        }
      }

      var resStage = processActions(input.before)
      if (looping && resStage.stages.actions.isEmpty) {
        resStage = resStage.copy(stages = resStage.initialStages)
        resStage = processActions(resStage)
      }

      input.withValue(resStage)
    }

    override def resource: Interp[T] = this

    def now(implicit scheduler: Scheduler): T = scheduler.singleReadValueOnce(this)
  }

  object Reactor {
    /** Creates a new Reactor, which steps through the reactor stages ones.
      *
      * @param initialValue The initial value of the Reactor.
      * @param stageBuilder The StageBuilder defining the Reactors behaviour.
      * @tparam T The type of the Reactor value.
      * @return The created Reactor.
      */
    def once[T](
        initialValue: T,
    )(stageBuilder: StageBuilder[T]): Reactor[T] = {
      CreationTicket.fromScheduler(scheduler)
        .create(
          Set(),
          new ReactorStage[T](initialValue, stageBuilder, stageBuilder),
          inite = true
        ) { createdState: State[ReactorStage[T]] =>
          new Reactor[T](createdState)
        }
    }

    /** Creates a new Reactor, which starts from the beginning, once it's finished.
      *
      * @param initialValue The initial value of the Reactor.
      * @param stageBuilder The StageBuilder defining the Reactors behaviour.
      * @tparam T The type of the Reactor value.
      * @return The created Reactor.
      */
    def loop[T](
        initialValue: T,
    )(stageBuilder: StageBuilder[T]): Reactor[T] = {
      CreationTicket.fromScheduler(scheduler)
        .create(
          Set(),
          new ReactorStage[T](initialValue, stageBuilder, stageBuilder),
          inite = true
        ) { createdState: State[ReactorStage[T]] =>
          new Reactor[T](createdState, true)
        }
    }
  }

  sealed trait ReactorAction[T]

  object ReactorAction {
    case class SetAction[T](res: T) extends ReactorAction[T]

    case class ModifyAction[T](modifier: T => T) extends ReactorAction[T]

    case class NextAction[T, E](event: Event[E], handler: E => StageBuilder[T])
        extends ReactorAction[T]

    case class ReadAction[T](stageBuilder: T => StageBuilder[T]) extends ReactorAction[T]
  }

  case class ReactorStage[T](currentValue: T, stages: StageBuilder[T], initialStages: StageBuilder[T])

  case class StageBuilder[T](actions: List[ReactorAction[T]] = Nil) {

    private def addAction(newValue: ReactorAction[T]): StageBuilder[T] = {
      copy(actions = actions :+ newValue)
    }

    /** Sets the value of the Reactor.
      *
      * @param newValue The new value of the Reactor.
      * @return A StageBuilder describing the Reactor behaviour.
      */
    def set(newValue: T): StageBuilder[T] = {
      addAction(ReactorAction.SetAction(newValue))
    }

    /** Modifies the value of the Reactor.
      *
      * @param modifier A function that has the old Reactor value as input and returns a new Reactor value.
      * @return A StageBuilder describing the Reactor behaviour.
      */
    def modify(modifier: T => T): StageBuilder[T] = {
      addAction(ReactorAction.ModifyAction(modifier))
    }

    /** Waits until the event is triggered.
      *
      * When the event is triggered the given body is executed in the
      * same transaction.
      *
      * @param event the event to wait for.
      * @param body  the code to execute when the event is triggered.
      * @tparam E the event's type.
      */
    def next[E](event: Event[E])(body: E => StageBuilder[T]): StageBuilder[T] = {
      addAction(ReactorAction.NextAction(event, body))
    }
    def next(event: Event[Unit])(body: => StageBuilder[T]): StageBuilder[T] = {
      addAction(ReactorAction.NextAction(event, (_: Unit) => body))
    }

    def read(stageBuilder: T => StageBuilder[T]): StageBuilder[T] = {
      addAction(ReactorAction.ReadAction(stageBuilder))
    }
  }
}
