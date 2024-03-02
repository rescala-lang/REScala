package reactives.core

/** Encapsulates an action changing a single source. */
trait InitialChange[State[_]] {

  /** The source to be changed. */
  val source: ReSource.of[State]

  /** @param base         the current (old) value of the source.
    * @param writeCallback callback to apply the new value, executed only if the action is approved by the source.
    * @return the propagation status of the source (whether or not to reevaluate output reactives).
    */
  def writeValue(base: source.Value, writeCallback: source.Value => Unit): Boolean
}

/** Enables reading of the current value during admission.
  * Keeps track of written sources internally.
  */
final class AdmissionTicket[State[_]](val tx: Transaction[State], declaredWrites: Set[ReSource.of[State]]) {

  private var _initialChanges: Map[ReSource.of[State], InitialChange[State]] =
    Map[ReSource.of[State], InitialChange[State]]()
  private[reactives] def initialChanges: Map[ReSource.of[State], InitialChange[State]] = _initialChanges
  def recordChange[T](ic: InitialChange[State]): Unit = {
    assert(
      declaredWrites.contains(ic.source),
      "must not set a source that has not been pre-declared for the transaction"
    )
    assert(!_initialChanges.contains(ic.source), "must not admit same source twice in one turn")
    _initialChanges += ic.source -> ic
  }

  /** convenience method as many case studies depend on this being available directly on the AT */
  def now[A](reactive: ReadAs.of[State, A]): A = tx.now(reactive)

  private[reactives] var wrapUp: Transaction[State] => Unit = null
}
