package rescala.engine

import rescala.graph.{AdmissionTicket, DynamicTicket, Pulsing, Reactive, StaticTicket, Struct, WrapUpTicket}

trait StateAccess[S <: Struct] extends CreationImpl[S] with Turn[S] {
  private[rescala] def staticBefore/*aka regRead/depRead*/[P](reactive: Pulsing[P, S]): P
  private[rescala] def staticAfter[P](reactive: Pulsing[P, S]): P
  private[rescala] def dynamicBefore[P](reactive: Pulsing[P, S]): P
  private[rescala] def dynamicAfter[P](reactive: Pulsing[P, S]): P
}

trait DirectAccessTicketsImpl[S <: Struct] extends StateAccess[S] {
  private[rescala] def makeDynamicReevaluationTicket(indeps: Set[Reactive[S]]): DynamicTicket[S] = new DynamicTicket[S](this, indeps)
  private[rescala] def makeStaticReevaluationTicket(): StaticTicket[S] = new StaticTicket[S](this)
  private[rescala] def makeAdmissionPhaseTicket(): AdmissionTicket[S] = new AdmissionTicket[S](this)
  private[rescala] def makeWrapUpPhaseTicket(): WrapUpTicket[S] = new WrapUpTicket[S](this)
}
