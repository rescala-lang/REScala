package rescala.core

/**
  * The Turn interface glues the reactive interface and the propagation implementation together.
  *
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
trait Turn[S <: Struct] extends ReevaluationStateAccess[S] {
  private[rescala] def makeDynamicReevaluationTicket(indeps: Set[ReSource[S]]): DynamicTicket[S]
  private[rescala] def makeStaticReevaluationTicket(): StaticTicket[S]
  private[rescala] def makeAdmissionPhaseTicket(): AdmissionTicket[S]
  private[rescala] def makeWrapUpPhaseTicket(): WrapUpTicket[S]

  /**
    * Registers a new handler function that is called after all changes were written and committed.
    *
    * @param f Handler function to register.
    */
  private[rescala] def observe(f: () => Unit): Unit
}



trait TurnImpl[S <: Struct] extends Turn[S] with CreationImpl[S] with ComputationStateAccess[S] {
  private[rescala] def makeDynamicReevaluationTicket(indeps: Set[ReSource[S]]): DynamicTicket[S] = new DynamicTicket[S](this, indeps)
  private[rescala] def makeStaticReevaluationTicket(): StaticTicket[S] = new StaticTicket[S](this)
  private[rescala] def makeAdmissionPhaseTicket(): AdmissionTicket[S] = new AdmissionTicket[S](this)
  private[rescala] def makeWrapUpPhaseTicket(): WrapUpTicket[S] = new WrapUpTicket[S](this)
}
