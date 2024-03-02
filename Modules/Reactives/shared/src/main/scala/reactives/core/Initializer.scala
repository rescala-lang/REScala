package reactives.core

/** An initializer is the glue between that binds the creation of the reactive from the operator and scheduler side together.
  * The operator provides the logic to wrap a state and the scheduler provides the implementation of that state.
  * This is where the two are joined. After that, the new reactive may have to be initialized.
  */
trait Initializer[S[_]] {

  /** Creates and correctly initializes new [[Derived]]s */
  final private[reactives] def create[V, T <: Derived.of[S]](
      incoming: Set[ReSource.of[S]],
      initialValue: V,
      needsReevaluation: Boolean
  )(instantiateReactive: S[V] => T): T = {
    val state    = makeDerivedStructState[V](initialValue)
    val reactive = instantiateReactive(state)
    register(reactive, incoming, initialValue)
    initialize(reactive, incoming, needsReevaluation)
    reactive
  }

  /** hook for schedulers to globally collect all created resources, usually does nothing */
  protected def register[V](reactive: ReSource.of[S], inputs: Set[ReSource.of[S]], initValue: V): Unit = {
    Tracing.observe(Tracing.Create(reactive, inputs.toSet, Tracing.ValueWrapper(initValue)))
  }

  /** Correctly initializes [[ReSource]]s */
  final private[reactives] def createSource[V, T <: ReSource.of[S]](initialValue: V)(instantiateReactive: S[V] => T)
      : T = {
    val state    = makeSourceStructState[V](initialValue)
    val reactive = instantiateReactive(state)
    register(reactive, Set.empty, initialValue)
    reactive
  }

  /** Creates the internal state of [[reactives.core.Derived]]s */
  protected def makeDerivedStructState[V](initialValue: V): S[V]

  /** Creates the internal state of [[ReSource]]s */
  protected def makeSourceStructState[V](initialValue: V): S[V] =
    makeDerivedStructState[V](initialValue)

  /** to be implemented by the propagation algorithm, called when a new reactive has been instantiated and needs to be connected to the graph and potentially reevaluated.
    *
    * @param reactive          the newly instantiated reactive
    * @param incoming          a set of incoming dependencies
    * @param needsReevaluation true if the reactive must be reevaluated at creation even if none of its dependencies change in the creating turn.
    */
  protected def initialize(
      reactive: Derived.of[S],
      incoming: Set[ReSource.of[S]],
      needsReevaluation: Boolean
  ): Unit

}
