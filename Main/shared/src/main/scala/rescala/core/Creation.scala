package rescala.core

trait Creation[S <: Struct] {
  /**
    * Connects a reactive element with potentially existing dependencies and prepares re-evaluations to be
    * propagated based on the turn's propagation scheme
    *
    * @param incoming a set of incoming dependencies
    * @param valuePersistency the value persistency
    * @param instantiateReactive The factory method to instantiate the reactive with the newly created state.
    * @tparam P Reactive value type
    * @return Connected reactive element
    */
  final private[rescala] def create[P, T <: Reactive[S]](incoming: Set[ReSource[S]], valuePersistency: ValuePersistency[P])(instantiateReactive: S#State[P, S] => T): T = {
    val state = makeDerivedStructState(valuePersistency)
    val reactive = instantiateReactive(state)
    ignite(reactive, incoming, valuePersistency.ignitionRequiresReevaluation)
    reactive
  }

  final private[rescala] def createSource[P, T <: ReSource[S]](valuePersistency: ValuePersistency[P])(instantiateReactive: S#State[P, S] => T): T = {
    val state = makeSourceStructState(valuePersistency)
    instantiateReactive(state)
  }

  /**
    * to be implemented by the scheduler, called when a new state storage object is required for instantiating a new reactive.
    * @param valuePersistency the value persistency
    * @tparam P the stored value type
    * @return the initialized state storage
    */
  protected[this] def makeDerivedStructState[P](valuePersistency: ValuePersistency[P]): S#State[P, S]

  /**
    * to be implemented by the scheduler, called when a new state storage object is required for instantiating a new reactive.
    * @param valuePersistency the value persistency
    * @tparam P the stored value type
    * @return the initialized state storage
    */
  protected[this] def makeSourceStructState[P](valuePersistency: ValuePersistency[P]): S#State[P, S] = makeDerivedStructState(valuePersistency)
  /**
    * to be implemented by the propagation algorithm, called when a new reactive has been instantiated and needs to be connected to the graph and potentially reevaluated.
    * @param reactive the newly instantiated reactive
    * @param incoming a set of incoming dependencies
    * @param ignitionRequiresReevaluation true if the reactive must be reevaluated at creation even if none of its dependencies change in the creating turn.
    */
  protected[this] def ignite(reactive: Reactive[S], incoming: Set[ReSource[S]], ignitionRequiresReevaluation: Boolean): Unit
}



