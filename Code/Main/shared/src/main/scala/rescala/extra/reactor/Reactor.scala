package rescala.extra.reactor

import rescala.core.ReName
import rescala.interface.RescalaInterface
import rescala.macros.MacroAccess

import scala.annotation.tailrec

class ReactorBundle[Api <: RescalaInterface](val api: Api) {
  import api._
  class Reactor[T](
      initState: State[ReactorStage[T]]
  ) extends Derived with Interp[T] with MacroAccess[T, Interp[T]] {

    override type Value = ReactorStage[T]

    override protected[rescala] def state: State[ReactorStage[T]] = initState
    override protected[rescala] def name: ReName                  = "Custom Reactor"
    override def interpret(v: ReactorStage[T]): T                 = v.currentValue
    override protected[rescala] def commit(base: Value): Value    = base

    /** called if any of the dependencies changed in the current update turn,
      * after all (known) dependencies are updated
      */
    override protected[rescala] def reevaluate(input: ReevTicket[ReactorStage[T]]): Result[ReactorStage[T]] = {

      input.trackDependencies(Set())

      @tailrec
      def processActions[A](stage: ReactorStage[T]): ReactorStage[T] = {
        stage.stages.actions match {
          case Nil => stage
          case ReactorAction.SetAction(v) :: tail =>
            processActions(stage.copy(currentValue = v, stages = StageBuilder(tail)))
          case ReactorAction.NextAction(event, handler) :: _ =>
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

    override def resource: Interp[T] = this

    def now(implicit scheduler: Scheduler): T = scheduler.forceNewTransaction(this)(at => at.now(this))

  }

  sealed trait ReactorAction[T]
  object ReactorAction {
    case class SetAction[T](res: T) extends ReactorAction[T]
    case class NextAction[T, E](event: Event[E], handler: E => StageBuilder[T])
        extends ReactorAction[T]
  }

  case class ReactorStage[T](currentValue: T, stages: StageBuilder[T])

  case class StageBuilder[T](actions: List[ReactorAction[T]] = Nil) {

    private def addAction(newValue: ReactorAction[T]): StageBuilder[T] = {
      copy(actions = actions :+ newValue)
    }

    def set(newValue: T): StageBuilder[T] = {
      addAction(ReactorAction.SetAction(newValue))
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
    def next[E](event: Event[E])(body: E => StageBuilder[T]): StageBuilder[T] = {
      addAction(ReactorAction.NextAction(event, body))
    }

  }
  object Reactor {
    def once[T](
        initialValue: T,
    )(stageBuilder: StageBuilder[T]): Reactor[T] = {
      CreationTicket.fromScheduler(scheduler)
        .create(
          Set(),
          new ReactorStage[T](initialValue, stageBuilder),
          inite = true
        ) { createdState: State[ReactorStage[T]] =>
          new Reactor[T](createdState)
        }
    }
  }
}
