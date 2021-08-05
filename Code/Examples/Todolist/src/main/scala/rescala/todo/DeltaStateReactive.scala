package rescala.todo

import rescala.core.ReName
import rescala.default._

case class DeltaWithState[Delta, State](delta: Seq[Delta], state: State)

class DeltaStateReactive[Delta, Combined](
    initState: State[DeltaWithState[Delta, Combined]],
    deltaInput: Interp[Delta],
    applyDelta: (Combined, Delta) => Combined,
    handlers: Seq[(DynamicTicket, Combined) => Delta],
    override protected[rescala] val name: ReName,
) extends Derived with Interp[DeltaWithState[Delta, Combined]] {
  override type Value = DeltaWithState[Delta, Combined]
  override protected[rescala] def state: State[Value]        = initState
  override protected[rescala] def commit(base: Value): Value = base.copy(delta = Nil)

  override protected[rescala] def reevaluate(input: ReIn): Rout = {
    input.trackDependencies(Set(deltaInput))
    val sourceVal     = input.dependStatic(deltaInput)
    var deltas        = List(sourceVal)
    val applyExternal = applyDelta(input.before.state, sourceVal)
    val combined = handlers.foldLeft(applyExternal) { case (current, handler) =>
      val delta = handler(input, current)
      deltas ::= delta
      applyDelta(current, delta)
    }

    input.withValue(DeltaWithState(deltas, combined))
  }
  override def interpret(v: DeltaWithState[Delta, Combined]): DeltaWithState[Delta, Combined] = v
}

object DeltaStateReactive {
  def create[State, Delta](
      init: State,
      deltaInput: Interp[Delta],
      applyDelta: (State, Delta) => State,
      handlers: Seq[(DynamicTicket, State) => Delta]
  )(implicit name: ReName, creationTicket: CreationTicket): DeltaStateReactive[Delta, State] =
    creationTicket.create(Set(deltaInput), DeltaWithState(List.empty[Delta], init), inite = false)(state =>
      new DeltaStateReactive(state, deltaInput, applyDelta, handlers, name)
    )
}
