package rescala.core

trait Creation[S <: Struct] {
  /** Creates and correctly initializes new [[Reactive]]s */
  final private[rescala] def create[P, T <: Reactive[S]]
  (incoming: Set[ReSource[S]], valuePersistency: ValuePersistency[P])(instantiateReactive: S#State[P, S] => T): T = {
    val state = makeDerivedStructState(valuePersistency)
    val reactive = instantiateReactive(state)
    ignite(reactive, incoming, valuePersistency.ignitionRequiresReevaluation)
    reactive
  }

  /** Correctly initializes [[ReSource]]s */
  final private[rescala] def createSource[P, T <: ReSource[S]]
  (valuePersistency: ValuePersistency[P])(instantiateReactive: S#State[P, S] => T): T = {
    val state = makeSourceStructState(valuePersistency)
    instantiateReactive(state)
  }

  /** Creates the internal state of [[Reactive]]s */
  protected[this] def makeDerivedStructState[P](valuePersistency: ValuePersistency[P]): S#State[P, S]

  /**  Creates the internal state of [[ReSourciV]]s */
  protected[this] def makeSourceStructState[P](valuePersistency: ValuePersistency[P]): S#State[P, S] =
    makeDerivedStructState(valuePersistency)
  /**
    * to be implemented by the propagation algorithm, called when a new reactive has been instantiated and needs to be connected to the graph and potentially reevaluated.
    * @param reactive the newly instantiated reactive
    * @param incoming a set of incoming dependencies
    * @param ignitionRequiresReevaluation true if the reactive must be reevaluated at creation even if none of its dependencies change in the creating turn.
    */
  protected[this] def ignite(reactive: Reactive[S], incoming: Set[ReSource[S]], ignitionRequiresReevaluation: Boolean): Unit
}



