package rescala.engine

import rescala.graph._

/**
  * The Turn interface glues the reactive interface and the propagation implementation together.
  *
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
trait Turn[S <: Struct] extends AlwaysTicket[S] {
  override val turn = this
  private[rescala] def makeDynamicReevaluationTicket(indeps: Set[Reactive[S]]): DynamicTicket[S] = new DynamicTicket[S](this, indeps)
  private[rescala] def makeStaticReevaluationTicket(): StaticTicket[S] = new StaticTicket[S](this)
  private[rescala] def makeAdmissionPhaseTicket(): AdmissionTicket[S] = new AdmissionTicket[S](this)
  private[rescala] def makeWrapUpPhaseTicket(): WrapUpTicket[S] = new WrapUpTicket[S](this)

  private[rescala] def staticBefore/*aka regRead/depRead*/[P](reactive: Pulsing[P, S]): P
  private[rescala] def staticAfter[P](reactive: Pulsing[P, S]): P
  private[rescala] def dynamicBefore[P](reactive: Pulsing[P, S]): P
  private[rescala] def dynamicAfter[P](reactive: Pulsing[P, S]): P

//  /**
//    * Synchronize for access (i.e., [[before]] or [[after]]) on this node when
//    * synchronization is unknown. Multiple invocations are redundant, but not harmful outside of an
//    * implementation-dependent performance penalty.
//    *
//    * @param reactive the reactive to be dynamically accessed
//    */
//  private[rescala] def dynamicDependencyInteraction(reactive: Reactive[S]): Unit
//
//  /**
//    * Read value from before this turn. Only call this if you know that you are synchronized with this node:
//    * Reads on dependencies where an edge exists (e.g., reading a static dependency) is always synchronized.
//    * Reads on other nodes must be synchronized through dynamicDependencyInteraction first.
//    * @param pulsing the node to be read
//    * @tparam P the node's storage type
//    * @return the stored value from before this turn
//    */
//  private[rescala] def before[P](pulsing: Pulsing[P, S]): P
//  /**
//    * Read value from after this turn. Implementations may return the node's current value, including
//    * changes already made by this turn but disregarding potential future changes, or may suspend to
//    * return only the value that is final until the end of this turn.
//    * Only call this if you know that you are synchronized with this node:
//    * Reads on dependencies where an edge exists (e.g., reading a static dependency) is always synchronized.
//    * Reads on other nodes must be synchronized through dynamicDependencyInteraction first.
//    * @param pulsing the node to be read
//    * @tparam P the node's storage type
//    * @return the stored value from after this turn
//    */
//  private[rescala] def after[P](pulsing: Pulsing[P, S]): P

  /**
    * Connects a reactive element with potentially existing dependencies and prepares re-evaluations to be
    * propagated based on the turn's propagation scheme
    *
    * @param incoming a set of incoming dependencies
    * @param valuePersistency the value persistency
    * @param instantiateReactive The factory method to instantiate the reactive with the newly created state.
    * @tparam P Reactive value type
    * @tparam R Reactive subtype of the reactive element
    * @return Connected reactive element
    */
  private[rescala] def create[P, R <: Reactive[S]](incoming: Set[Reactive[S]], valuePersistency: ValuePersistency[P])(instantiateReactive: S#State[P, S] => R): R

  /**
    * Registers a new handler function that is called after all changes were written and committed.
    *
    * @param f Handler function to register.
    */
  private[rescala] def observe(f: () => Unit): Unit
}

