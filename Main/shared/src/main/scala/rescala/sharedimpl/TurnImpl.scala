package rescala.sharedimpl

import rescala.core.{AdmissionTicket, ComputationStateAccess, Creation, DynamicTicket, ReSource, StaticTicket, Struct, WrapUpTicket}

trait TurnImpl[S <: Struct] extends Creation[S] with ComputationStateAccess[S] with ReevaluationStateAccess[S] {
  private[rescala] def makeDynamicReevaluationTicket(indeps: Set[ReSource[S]]): DynamicTicket[S] = new DynamicTicket[S](this, indeps)
  private[rescala] def makeStaticReevaluationTicket(): StaticTicket[S] = new StaticTicket[S](this)
  private[rescala] def makeAdmissionPhaseTicket(): AdmissionTicket[S] = new AdmissionTicket[S](this)
  private[rescala] def makeWrapUpPhaseTicket(): WrapUpTicket[S] = new WrapUpTicket[S](this)
}
