package rescala.core

trait Initializer[S <: Struct] {

  /** Creates and correctly initializes new [[rescala.core.Derived]]s */
  final private[rescala] def create[V, T <: Derived[S]](
      incoming: Set[ReSource[S]],
      initv: V,
      inite: Boolean,
      creationTicket: CreationTicket[S]
  )(instantiateReactive: S#State[V, S] => T): T = {
    val state    = makeDerivedStructState[V](initv)
    val reactive = instantiateReactive(state)
    register(reactive)
    ignite(reactive, incoming, inite)
    reactive
  }

  def accessTicket(): AccessTicket[S]

  protected[this] def register(reactive: ReSource[S]): Unit = ()

  /** Correctly initializes [[ReSource]]s */
  final private[rescala] def createSource[V, T <: ReSource[S]](
      intv: V,
      creationTicket: CreationTicket[S]
  )(instantiateReactive: S#State[V, S] => T): T = {
    val state    = makeSourceStructState[V](intv)
    val reactive = instantiateReactive(state)
    register(reactive)
    reactive
  }

  /** Creates the internal state of [[Derived]]s */
  protected[this] def makeDerivedStructState[V](valuePersistency: V): S#State[V, S]

  /** Creates the internal state of [[ReSource]]s */
  protected[this] def makeSourceStructState[V](valuePersistency: V): S#State[V, S] =
    makeDerivedStructState[V](valuePersistency)

  /**
    * to be implemented by the propagation algorithm, called when a new reactive has been instantiated and needs to be connected to the graph and potentially reevaluated.
    *
    * @param reactive                     the newly instantiated reactive
    * @param incoming                     a set of incoming dependencies
    * @param ignitionRequiresReevaluation true if the reactive must be reevaluated at creation even if none of its dependencies change in the creating turn.
    */
  protected[this] def ignite(
      reactive: Derived[S],
      incoming: Set[ReSource[S]],
      ignitionRequiresReevaluation: Boolean
  ): Unit

}
