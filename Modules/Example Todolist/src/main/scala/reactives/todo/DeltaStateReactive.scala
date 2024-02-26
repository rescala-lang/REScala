package reactives.todo

import reactives.core.{CreationTicket, Derived, DynamicTicket, ReInfo, ReadAs}
import reactives.default.*

case class DeltaWithState[Delta, DState](delta: Seq[Delta], state: DState)

class DeltaStateReactive[Delta, Combined](
    initState: BundleState[DeltaWithState[Delta, Combined]],
    deltaInput: ReadAs.of[BundleState, Delta],
    applyDelta: (Combined, Delta) => Combined,
    handlers: Seq[(DynamicTicket[BundleState], Combined) => Delta],
    override val info: ReInfo,
) extends Derived with ReadAs[DeltaWithState[Delta, Combined]] {
  override type Value    = DeltaWithState[Delta, Combined]
  override type State[V] = reactives.default.BundleState[V]
  override protected[reactives] def state: State[Value]        = initState
  override protected[reactives] def commit(base: Value): Value = base.copy(delta = Nil)

  override protected[reactives] def reevaluate(input: ReIn): Rout = {
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
  override def read(v: DeltaWithState[Delta, Combined]): DeltaWithState[Delta, Combined] = v
}

object DeltaStateReactive {
  def create[DState, Delta](
      init: DState,
      deltaInput: ReadAs.of[BundleState, Delta],
      applyDelta: (DState, Delta) => DState,
      handlers: Seq[(DynamicTicket[BundleState], DState) => Delta]
  )(implicit name: ReInfo, creationTicket: CreationTicket[BundleState]): DeltaStateReactive[Delta, DState] =
    creationTicket.create(Set(deltaInput), DeltaWithState(List.empty[Delta], init), needsReevaluation = false)(state =>
      new DeltaStateReactive(state, deltaInput, applyDelta, handlers, name)
    )
}
